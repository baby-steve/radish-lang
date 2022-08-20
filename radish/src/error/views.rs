use crate::common::Span;
use crate::error::{renderer::Renderer, Diagnostic, Label, LabelStyle};
use std::io;

pub struct RichDiagnostic<'diagnostic> {
    diagnostic: &'diagnostic Diagnostic,
}

impl<'diagnostic> RichDiagnostic<'diagnostic> {
    pub fn new(diagnostic: &'diagnostic Diagnostic) -> RichDiagnostic<'diagnostic> {
        RichDiagnostic { diagnostic }
    }

    pub fn render(&self, renderer: &mut Renderer) -> io::Result<()> {
        renderer.render_header(self.diagnostic.severity, &self.diagnostic.message)?;

        // sort the labels based on their line number so that they get 
        // rendered in the correct order.
        let mut sorted_labels = self.diagnostic.labels.clone();
        sorted_labels.sort_by(|a, b| a.span.start.cmp(&b.span.start));

        let mut primary_label: Option<&Label> = None;

        // the line that we've seen
        let mut last_line: Option<usize> = None;

        // keep track of which line has which labels
        let mut all_labels: Vec<Vec<&Label>> = vec![];
        let mut current: Vec<&Label> = vec![];

        for label in sorted_labels.iter() {
            if label.style == LabelStyle::Primary {
                primary_label = Some(label);
            }

            // get the label's line number. For now just persume its single lined.
            let start = label.span.start;
            let (start_line, _start_col) =
                Span::get_line_index(&label.span.source.contents, start);

            // have we seen this line before?
            if last_line == None || last_line.unwrap() == start_line {
                // yup we've seen it before, so go ahead and just add it to the current
                // group of labels.
                current.push(label);
            } else {
                // nope, haven't seen it before.
                // add the last group of labels to the collection of all labels
                all_labels.push(current);
                // add this label to a new group of labels.
                current = vec![label];
            }

            last_line = Some(start_line);
        }

        // push the last group of labels to the collection of all labels
        all_labels.push(current);

        let padding = last_line.unwrap().to_string().len();

        // render this diagnostics's location
        if let Some(label) = primary_label {
            renderer.render_location(&label.span, padding)?;
        } else {
            // TODO: handle when a diagnostic doesn't have a primary 
            // label (or any label for that matter).
            unimplemented!("couldn't find a primary label");
        }

        renderer.render_empty_line(padding + 1)?;

        // render each source line and their labels
        let mut previous_group: Option<&Vec<&Label>> = None;
        for group in all_labels.iter() {
            let start = group[0].span.start;
            let (start_line, _) =
                Span::get_line_index(&group[0].span.source.contents, start);

            // if there was a gap between this line and the last, render a line break.
            if let Some(prev) = previous_group {
                let prev_start = prev[0].span.start;
                let (prev_start_line, _) =
                    Span::get_line_index(&prev[0].span.source.contents, prev_start);

                if prev_start_line + 1 != start_line {
                    renderer.render_line_break(padding)?;
                }
            };

            renderer.render_snippet_source(
                padding,
                start_line,
                &group[0].span.source.contents,
                group,
                self.diagnostic.severity,
            )?;

            previous_group = Some(group);
        }

        renderer.render_empty_line(padding + 1)?;

        // render any notes this diagnostic may have.
        for note in self.diagnostic.notes.iter() {
            renderer.render_snippet_note(note, padding)?;
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

    pub fn render(&self, renderer: &mut Renderer<'_>) -> io::Result<()> {
        // Located header
        //
        // ```text
        // error: some error message
        // ```
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
