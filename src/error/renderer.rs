//use std::io::{self};
use termcolor::WriteColor;

use crate::common::span::Span;
use crate::error::diagnostic::Severity;

pub struct Renderer<'writer> {
    pub writer: &'writer mut dyn WriteColor,
}

// TODO: replace 'unwraps' with errors or such.

impl<'writer> Renderer<'writer> {
    pub fn new(writer: &'writer mut dyn WriteColor) -> Renderer<'writer> {
        Renderer { writer }
    }

    /// Render diagnostic's severity level and message
    ///
    /// ```text
    /// error: some error message
    /// ```
    pub fn render_header(&mut self, severity: Severity, message: &str) -> Result<(), String> {
        // write severity level name
        match severity {
            Severity::Bug => write!(self.writer, "bug").unwrap(),
            Severity::Error => write!(self.writer, "error").unwrap(),
            Severity::Warning => write!(self.writer, "warning").unwrap(),
            Severity::Help => write!(self.writer, "help").unwrap(),
            Severity::Note => write!(self.writer, "note").unwrap(),
        };

        // write message
        write!(self.writer, ": {}", message).unwrap();

        writeln!(self.writer).unwrap();

        Ok(())
    }

    /// Render diagnostic's location
    ///
    /// ```text
    ///  --> path/to/file.rdsh:12:3
    /// ```
    pub fn render_location(&mut self, span: &Span, padding: usize) -> Result<(), String> {
        let (line_num, col_num) = Span::get_line_index(&span.source.contents, span.start)
            .expect("Could not find line and column numbers");

        write!(
            self.writer,
            "{}--> {name}:{line_num}:{col_num}",
            " ".repeat(padding),
            name = span.source.as_ref().clone().path.to_string_lossy(),
            line_num = line_num,
            col_num = col_num,
        ).unwrap();

        writeln!(self.writer).unwrap();

        Ok(())
    }

    pub fn render_snippet_source(
        &mut self,
        outer_padding: usize,
        line_number: usize,
        source: &str,
        _severity: Severity,
    ) -> Result<(), String> {
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
            write!(self.writer, " ").unwrap();

            // Write source text.
            let lines = Span::lines(&source);
            write!(self.writer, "{}", lines[line_number]).unwrap();

            writeln!(self.writer).unwrap();
        }

        Ok(())
    }

    fn outer_gutter_number(&mut self, line_num: usize, padding: usize) -> Result<(), String> {
        write!(
            self.writer,
            "{line_num: >width$}",
            line_num = line_num,
            width = padding,
        ).unwrap();
        write!(self.writer, " ").unwrap();
        Ok(())
    }

    fn border_left(&mut self) -> Result<(), String> {
        write!(self.writer, "|").unwrap();
        Ok(())
    }

    pub fn render_snippet_note(&mut self, _padding: usize, _note: &str) -> Result<(), String> {
        Ok(())
    }
}
