use crate::common::span::Span;
use crate::error::{renderer::Renderer, Diagnostic, LabelStyle};

pub struct RichDiagnostic<'diagnostic> {
    diagnostic: &'diagnostic Diagnostic,
}

impl<'diagnostic> RichDiagnostic<'diagnostic> {
    pub fn new(diagnostic: &'diagnostic Diagnostic) -> RichDiagnostic<'diagnostic> {
        RichDiagnostic { diagnostic }
    }

    pub fn render(&self, renderer: &mut Renderer) -> Result<(), String> {
        renderer.render_header(self.diagnostic.severity, &self.diagnostic.message)?;
        //let labels = self.diagnostic.labels.iter();

        for label in self.diagnostic.labels.iter() {
            let start = label.span.start;
            let end = label.span.end;

            let contents = label.span.source.contents.clone();
            //let lines = Span::lines(&contents);

            let (start_line, _start_col) = Span::get_line_index(&contents, start).unwrap();
            let (end_line, _end_col) = Span::get_line_index(&contents, end).unwrap();

            //let readable_start_line = (start_line + 1).to_string();
            let readable_end_line = (end_line + 1).to_string();
            //let readable_start_col = (start_col + 1).to_string();
            let padding = readable_end_line.len(); // FIXME: calculate from last label.

            if label.style == LabelStyle::Primary {
                renderer.render_location(&label.span, padding)?;
            }

            // render some lines before snippet source for context?

            renderer.render_snippet_source(
                padding, 
                start_line, 
                &contents, 
                self.diagnostic.severity,
            )?;
        }

        Ok(())
    }
}

/// Output a short diagnostic, with a line number, severity, and message.
pub struct ShortDiagnostic<'diagnostic> {
    diagnostic: &'diagnostic Diagnostic,
    //show_notes: bool,
}

impl<'diagnostic> ShortDiagnostic<'diagnostic> {
    pub fn new(
        diagnostic: &'diagnostic Diagnostic,
        //show_notes: bool,
    ) -> ShortDiagnostic<'diagnostic> {
        ShortDiagnostic {
            diagnostic,
            //show_notes,
        }
    }

    pub fn render(&self, renderer: &mut Renderer<'_>) -> Result<(), String> {
        // Located header/s
        //
        // ```text
        // error: some e/rror me/ssag/e
        // ```/
        let mut primary_labels_encountered = 0;
        let labels = self.diagnostic.labels.iter();
        for label in labels.filter(|label| label.style == LabelStyle::Primary) {
            primary_labels_encountered += 1;

            renderer.render_header(self.diagnostic.severity, &label.message)?;

            renderer.render_location(&label.span, 0)?;
        }

        // Fallback to printing a non-located header if no primary labels were encountered
        //
        // ```text
        // error[E0002]: Bad config found
        // ```
        if primary_labels_encountered == 0 {
            renderer.render_header(self.diagnostic.severity, &self.diagnostic.message)?;
        }

        /*if self.show_notes {
            // Additional notes
            //
            // ```text
            // = expected type `Int`
            //      found type `String`
            // ```
            for note in &self.diagnostic.notes {
                renderer.render_snippet_note(0, note)?;
            }
        }*/

        Ok(())
    }
}
/*
#[cfg(test)]
mod tests {
    use super::*;
    use termcolor::{WriteColor, ColorSpec};
    use std::io;

    pub struct TestWriter {
        buf: Vec<u8>,
        color: ColorSpec,
    }

    impl io::Write for TestWriter {
        fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error> {
            self.buf.extend(buf);
            Ok(self.buf.len())
        }

        fn flush(&mut self) -> Result<(), io::Error> {
            Ok(())
        }
    }

    impl WriteColor for TestWriter {
        fn supports_color(&self) -> bool {
            true
        }
    
        fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
            #![allow(unused_assignments)]
    
            if self.color == *spec {
                return Ok(());
            } else {
                self.color = spec.clone();
            }
    
            if spec.is_none() {
                write!(self, "{{/}}")?;
                return Ok(());
            } else {
                write!(self, "{{")?;
            }
    
            let mut first = true;
    
            fn write_first(first: bool, write: &mut TestWriter) -> io::Result<bool> {
                if !first {
                    write!(write, " ")?;
                }
    
                Ok(false)
            }
    
            if let Some(fg) = spec.fg() {
                first = write_first(first, self)?;
                write!(self, "fg:{:?}", fg)?;
            }
    
            if let Some(bg) = spec.bg() {
                first = write_first(first, self)?;
                write!(self, "bg:{:?}", bg)?;
            }
    
            if spec.bold() {
                first = write_first(first, self)?;
                write!(self, "bold")?;
            }
    
            if spec.underline() {
                first = write_first(first, self)?;
                write!(self, "underline")?;
            }
    
            if spec.intense() {
                first = write_first(first, self)?;
                write!(self, "bright")?;
            }
    
            write!(self, "}}")?;
    
            Ok(())
        }
    
        fn reset(&mut self) -> io::Result<()> {
            let color = self.color.clone();
    
            if color != ColorSpec::new() {
                write!(self, "{{/}}")?;
                self.color = ColorSpec::new();
            }
    
            Ok(())
        }
    }

    
}*/