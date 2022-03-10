//! Implemention of rustyline's helper for Radish's REPL.

use colored::*;

use std::borrow::Cow;
use std::collections::HashSet;

use rustyline::{
    completion::Completer,
    highlight::Highlighter,
    hint::{Hint, Hinter},
    validate::Validator,
    Context, Helper,
};

pub struct RadishRustylineHelper {
    // FIXME: use ** radix trie **
    hints: HashSet<CommandHint>,
}

impl RadishRustylineHelper {
    pub fn new(hints: HashSet<CommandHint>) -> Self {
        RadishRustylineHelper { hints }
    }
}

#[derive(Hash, Debug, PartialEq, Eq)]
pub struct CommandHint {
    display: String,
    complete_up_to: usize,
}

impl Hint for CommandHint {
    fn display(&self) -> &str {
        &self.display
    }

    fn completion(&self) -> Option<&str> {
        if self.complete_up_to > 0 {
            Some(&self.display[..self.complete_up_to])
        } else {
            None
        }
    }
}

impl CommandHint {
    fn new(text: &str, complete_up_to: &str) -> CommandHint {
        assert!(text.starts_with(complete_up_to));
        CommandHint {
            display: text.into(),
            complete_up_to: complete_up_to.len(),
        }
    }

    fn suffix(&self, strip_chars: usize) -> CommandHint {
        CommandHint {
            display: self.display[strip_chars..].to_owned(),
            complete_up_to: self.complete_up_to.saturating_sub(strip_chars),
        }
    }
}

impl Hinter for RadishRustylineHelper {
    type Hint = CommandHint;

    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<CommandHint> {
        if line.is_empty() || pos < line.len() {
            return None;
        }

        self.hints
            .iter()
            .filter_map(|hint| {
                // expect hint after word complete, like redis cli, add condition:
                // line.ends_with(" ")
                if hint.display.starts_with(line) {
                    Some(hint.suffix(pos))
                } else {
                    None
                }
            })
            .next()
    }
}

impl Highlighter for RadishRustylineHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        hint.truecolor(70, 74, 79).to_string().into()
    }
}

impl Validator for RadishRustylineHelper {}

impl Helper for RadishRustylineHelper {}

impl Completer for RadishRustylineHelper {
    type Candidate = String;
}

// TODO: make this less of a hack.
pub fn register_hints() -> HashSet<CommandHint> {
    let mut set = HashSet::new();
    set.insert(CommandHint::new("while", "while"));
    set.insert(CommandHint::new("fun", "fun"));
    set
}