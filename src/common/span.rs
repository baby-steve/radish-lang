use std::{
    fmt,
    rc::Rc,
};

use crate::common::source::Source;

#[derive(Clone, PartialEq)]
pub struct Span {
    pub source: Rc<Source>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(source: Rc<Source>, start: usize, end: usize) -> Span {
        Span { source, start, end }
    }

    pub fn from(span: &Span) -> Span {
        Span {
            source: Rc::clone(&span.source),
            end: span.end,
            start: span.start,
        }
    }

    pub fn empty() -> Span {
        let source = Source::source("");
        Span {
            source: Rc::clone(&source),
            start: 0,
            end: 0,
        }
    }

    pub fn combine(span_1: &Span, span_2: &Span) -> Span {
        if span_1.source != span_2.source {
            panic!("Cannot combine two Spans from different sources.");
        }

        let start = if span_1.start <= span_2.start {
            span_1.start
        } else {
            span_2.start
        };

        let end = if span_1.end >= span_2.end {
            span_1.end
        } else {
            span_2.end
        };

        Span {
            source: Rc::clone(&span_1.source),
            start,
            end,
        }
    }

    fn lines_newline(string: &str) -> Vec<String> {
        string.split('\n').map(|l| l.to_string() + "\n").collect()
    }

    fn lines(string: &str) -> Vec<String> {
        string.split('\n').map(|l| l.to_string()).collect()
    }

    pub fn get_line_index(string: &str, index: usize) -> Option<(usize, usize)> {
        let lines = Span::lines_newline(&string[..index]);
        let line = lines.len() - 1;
        let col = lines.last().unwrap().chars().count() - 1;

        Some((line, col))
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span({}, {})", self.start, self.end)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        /*
         *   --> source/path:line:col
         *    |
         * 23 | x = error
         *    |     ^^^^^
         *
         * things like the error message get handled by the thing that
         * raised the error; the span's job is to just show the location.
         *
         * A full error message would look something like:
         *
         * error[code]: some sort of error message
         *  --> path/to/file:4:12
         *   |
         * 4 | x = call().resolt
         *   |            ^^^^^^
         *  - help: did you mean `result`?
         *  - note: some kind of note.
         */

        let contents = self.source.contents.clone();
        let lines = Span::lines(&contents);

        let (start_line, start_col) = Span::get_line_index(&contents, self.start).unwrap();

        let (end_line, _end_col) = Span::get_line_index(&contents, self.end).unwrap();

        let readable_start_line = (start_line + 1).to_string();
        let readable_end_line = (end_line + 1).to_string();
        let readable_start_col = (start_col + 1).to_string();
        let padding = readable_end_line.len();

        let location = format!(
            " {}--> {}:{}:{}",
            " ".repeat(padding),
            self.source.as_ref().clone().path.to_string_lossy(),
            readable_start_line,
            readable_start_col
        );

        let separator = format!(" {} |", " ".repeat(padding));

        if start_line == end_line {
            let line = &lines[start_line];

            let excerpt = format!(" {} | {}", readable_end_line, line);

            let span = format!(
                " {} | {}{}",
                " ".repeat(padding),
                " ".repeat(start_col),
                "^".repeat(self.end - self.start),
            );

            writeln!(f, "{}", location)?;
            writeln!(f, "{}", separator)?;
            writeln!(f, "{}", excerpt)?;
            writeln!(f, "{}", span)
        } else {
            let mut formatted = vec![];

            for (i, line) in lines.iter().enumerate() {
                let readable_line_no = (start_line + i + 1).to_string();
                let partial_padding = " ".repeat(padding - readable_line_no.len());
                formatted.push(format!(
                    " {}{} |> {}",
                    partial_padding, readable_line_no, line
                ));
            }

            writeln!(f, "{}", location)?;
            writeln!(f, "{}", separator)?;
            writeln!(f, "{}", formatted.join("\n"))?;
            writeln!(f, "{}", separator)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_get_line_index() {
        let test_source = "hello, world!";
        //                        ^------ get this position (0:7)
        let pos = Span::get_line_index(test_source, 7);
        assert_eq!(pos, Some((0, 7)));

        let test_source = "hello,\n world!";
        //                          ^------ get this position (1:1)
        let pos = Span::get_line_index(test_source, 8);
        assert_eq!(pos, Some((1, 1)));
    }

    #[test]
    fn display_single_line_span() {
        let test_source = "1 + 2 * 3";
        let test_span = Span::new(Source::source(test_source), 4, 9);
        assert_eq!(
            format!("{}", test_span),
            "  --> ./main:1:5\n   |\n 1 | 1 + 2 * 3\n   |     ^^^^^\n"
        )
    }

    #[test]
    fn display_single_char_span() {
        let test_source = "1 + 2 * 3";
        let test_span = Span::new(Source::source(test_source), 0, 1);
        assert_eq!(
            format!("{}", test_span),
            "  --> ./main:1:1\n   |\n 1 | 1 + 2 * 3\n   | ^\n"
        )
    }

    #[test]
    fn combine_span() {
        let source = "Hello, Radish!";
        let span1 = Span::new(Source::source(source), 0, 5);
        let span2 = Span::new(Source::source(source), 7, 10);

        let combine_span = Span::combine(&span1, &span2);
        assert_eq!((combine_span.start, combine_span.end), (0, 10));

        let combine_span = Span::combine(&span2, &span1);
        assert_eq!((combine_span.start, combine_span.end), (0, 10));
    }
}
