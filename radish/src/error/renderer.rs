//use std::io::{self};
use termcolor::{Color, ColorSpec, WriteColor};

use crate::common::span::Span;
use crate::error::diagnostic::{Label, LabelStyle, Severity};
use std::io::{self, Write};

#[derive(Default)]
pub struct Styles {
    header_bug: ColorSpec,
    header_error: ColorSpec,
    header_warning: ColorSpec,
    header_note: ColorSpec,
    header_message: ColorSpec,
    header_help: ColorSpec,
    source_border: ColorSpec,
    primary_label: ColorSpec,
    secondary_label: ColorSpec,
}

impl Styles {
    pub fn header(&self, severity: Severity) -> &ColorSpec {
        match severity {
            Severity::Bug => &self.header_bug,
            Severity::Error => &self.header_error,
            Severity::Warning => &self.header_warning,
            Severity::Note => &self.header_note,
            Severity::Help => &self.header_help,
        }
    }

    pub fn new() -> Styles {
        let mut header = ColorSpec::new().set_bold(true).set_intense(true).clone();

        Styles {
            header_bug: header.set_fg(Some(Color::Magenta)).clone(),
            header_error: header.set_fg(Some(Color::Red)).clone(),
            header_warning: header.set_fg(Some(Color::Yellow)).clone(),
            header_note: header.set_fg(Some(Color::Green)).clone(),
            header_message: header.set_fg(Some(Color::White)).clone(),
            header_help: header.set_fg(Some(Color::Cyan)).clone(),
            source_border: ColorSpec::new()
                .set_fg(Some(Color::Blue))
                .set_bold(true)
                .clone(),
            primary_label: header.set_fg(Some(Color::Red)).clone(),
            secondary_label: header.set_fg(Some(Color::Yellow)).clone(),
        }
    }
}

pub struct Renderer<'writer> {
    pub writer: &'writer mut dyn WriteColor,
    styles: Styles,
}

// TODO: replace 'unwraps' with errors or such.

impl<'writer> Renderer<'writer> {
    pub fn new(writer: &'writer mut dyn WriteColor) -> Renderer<'writer> {
        Renderer {
            writer,
            styles: Styles::new(),
        }
    }

    pub fn styles(&self) -> &Styles {
        &self.styles
    }

    /// Render diagnostic's severity level and message
    ///
    /// ```text
    /// error: some error message
    /// ```
    pub fn render_header(&mut self, severity: Severity, message: &str) -> io::Result<()> {
        //self.set_color(self.styles().header(severity))?;
        self.set_color(&self.styles().header(severity).clone())?;

        // write severity level name
        match severity {
            Severity::Bug => write!(self, "bug")?,
            Severity::Error => write!(self, "error")?,
            Severity::Warning => write!(self, "warning")?,
            Severity::Help => write!(self, "help")?,
            Severity::Note => write!(self, "note")?,
        };

        // write message
        self.set_color(&self.styles().header_message.clone())?;
        write!(self, ": {}", message)?;

        self.reset()?;

        writeln!(self)?;

        Ok(())
    }

    /// Render diagnostic's location
    ///
    /// ```text
    ///  --> path/to/file.rdsh:12:3
    /// ```
    pub fn render_location(&mut self, span: &Span, padding: usize) -> io::Result<()> {
        let (line_num, col_num) = Span::get_line_index(&span.source.contents, span.start);

        self.set_color(&self.styles().source_border.clone())?;

        write!(self, "{}--> ", " ".repeat(padding),)?;

        self.reset()?;

        write!(
            self,
            "{name}:{line_num}:{col_num}",
            name = span.source.as_ref().clone().path.to_string_lossy(),
            line_num = line_num + 1,
            col_num = col_num + 1,
        )?;

        writeln!(self)?;

        Ok(())
    }

    pub fn render_snippet_source(
        &mut self,
        outer_padding: usize,
        line_number: usize,
        source: &str,
        labels: &[&Label],
        _severity: Severity,
    ) -> io::Result<()> {
        // Write source line
        //
        // e.g
        // 23 | some line of source code.
        {
            // 23
            self.outer_gutter_number(line_number, outer_padding)?;
            // 23 |
            self.border_left()?;

            // Write the inner gutter.
            write!(self, " ")?;

            // Write source text.
            let lines = Span::lines(&source);
            write!(self, "{}", lines[line_number])?;

            writeln!(self)?;
        }

        {
            let padding = outer_padding + 1;

            // the end of the last label
            let mut end_of_last = 0;

            self.outer_gutter(padding)?;
            self.border_left()?;

            for label in labels {
                let start = label.span.start;
                let end = label.span.end;

                let (_, start_col) = Span::get_line_index(&label.span.source.contents, start);
                let (_, end_col) = Span::get_line_index(&label.span.source.contents, end);

                let label = if label.style == LabelStyle::Primary {
                    self.set_color(&self.styles().primary_label.clone())?;
                    "^".repeat(label.span.end - start)
                } else {
                    self.set_color(&self.styles().secondary_label.clone())?;
                    "-".repeat(label.span.end - start)
                };

                let space_before = " ".repeat(start_col - end_of_last + 1);
                
                write!(self, "{}{}", space_before, label)?;

                end_of_last = end_col + 1;
            }

            // render the trailing label
            write!(self, " {}\n", labels.last().unwrap().message)?;

            // render the previous label if there was one
            if labels.len() == 2 {
                let label = labels[0];

                self.outer_gutter(padding)?;
                self.border_left()?;

                let (_, start_col) =
                    Span::get_line_index(&label.span.source.contents, label.span.start);

                let space_before = " ".repeat(start_col + 1);
                write!(self, "{}|\n", space_before).unwrap();

                self.outer_gutter(padding)?;
                self.border_left()?;

                write!(self, "{}{}", space_before, label.message).unwrap();

                writeln!(self).unwrap();
            }

            self.reset()?;

            // render any hanging labels
            // number of remaining labels to be rendered
            /*let mut remaining = labels.len();
            loop {
                // if this is the last label, exit the loop. Don't render it
                if remaining == 1 {
                    break;
                }
                self.outer_gutter_number(line_number, outer_padding)?;
                self.border_left()?;

                let mut end_of_last = 0;

                for label in labels[..remaining - 1].iter() {
                    let (_, end_col) =
                        Span::get_line_index(&label.span.source.contents, label.span.end).unwrap();

                    let space_before = " ".repeat(end_col - end_of_last);
                    write!(self.writer, "{}|", space_before).unwrap();

                    end_of_last = end_col + 1;
                }

                if labels.len() != remaining {
                    write!(self.writer, " {}", labels[remaining - 2].message).unwrap();
                    writeln!(self.writer).unwrap();
                }

                remaining -= 1;
            }*/
        }

        Ok(())
    }

    pub fn render_line_break(&mut self, padding: usize) -> io::Result<()> {
        self.set_color(&self.styles().source_border.clone())?;

        writeln!(self, "{} .", " ".repeat(padding))?;

        self.reset()?;

        Ok(())
    }

    pub fn render_snippet_note(&mut self, note: &str, padding: usize) -> io::Result<()> {
        self.outer_gutter(padding)?;

        self.set_color(&self.styles().source_border.clone())?;
        write!(self, " = ")?;

        self.set_color(&self.styles().header_note.clone())?;
        write!(self, "note: ")?;

        self.reset()?;
        write!(self, "{}", note)?;

        writeln!(self)?;

        Ok(())
    }

    pub fn render_empty_line(&mut self, padding: usize) -> io::Result<()> {
        self.outer_gutter(padding)?;
        self.border_left()?;

        writeln!(self)?;

        Ok(())
    }

    fn outer_gutter_number(&mut self, line_num: usize, padding: usize) -> io::Result<()> {
        self.set_color(&self.styles().source_border.clone())?;
        write!(
            self,
            "{line_num: >width$}",
            line_num = line_num + 1,
            width = padding,
        )?;
        write!(self, " ")?;
        self.reset()?;
        Ok(())
    }

    fn outer_gutter(&mut self, padding: usize) -> io::Result<()> {
        write!(self, "{}", " ".repeat(padding))?;

        Ok(())
    }

    fn border_left(&mut self) -> io::Result<()> {
        self.set_color(&self.styles().source_border.clone())?;
        write!(self, "|")?;
        self.reset()?;

        Ok(())
    }
}

impl<'writer> Write for Renderer<'writer> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

impl<'writer> WriteColor for Renderer<'writer> {
    fn supports_color(&self) -> bool {
        self.writer.supports_color()
    }

    fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
        self.writer.set_color(spec)
    }

    fn reset(&mut self) -> io::Result<()> {
        self.writer.reset()
    }

    fn is_synchronous(&self) -> bool {
        self.writer.is_synchronous()
    }
}
