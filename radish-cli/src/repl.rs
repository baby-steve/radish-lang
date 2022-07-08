//! REPL for the Radish programming language.

use radish::{RadishError, VM};

use rustyline::{error::ReadlineError, Editor};

use crate::hint::{register_hints, RadishRustylineHelper};

use term_size;

/// Repl's line prompt character.
const PROMPT: &str = "> ";
/// line countinuation character.
const CONTINUE: &str = "| ";

/// help command.
const HELP: &str = ".help";

enum ReplResult<T> {
    Ok(T),
    Error(RadishError),
    Eof,
    Interrupted,
}

pub struct Repl<'a> {
    vm: VM,
    lines: Vec<String>,
    prev_len: usize,
    prompt: &'a str,
}

impl<'a> Repl<'a> {
    pub fn new(vm: VM) -> Self {
        Repl {
            vm,
            lines: vec![],
            prev_len: 0,
            prompt: PROMPT,
        }
    }

    /// Fire up the REPL
    pub fn run(&mut self) {
        self.print_welcome();

        let helper = RadishRustylineHelper::new(register_hints());

        let mut editor = Editor::<RadishRustylineHelper>::new();
        editor.set_helper(Some(helper));

        loop {
            match self.readline(&mut editor) {
                ReplResult::Ok(_) => {
                    self.reset();
                    continue;
                }
                ReplResult::Error(err) => {
                    err.emit();
                    self.lines.truncate(self.prev_len);
                    self.reset();
                    continue;
                }
                ReplResult::Eof => {
                    println!("Goodbye!");
                    break;
                }
                ReplResult::Interrupted => {
                    self.reset();
                    continue;
                }
            }
        }
    }

    /// Read a line, also handling multiline input.
    fn readline(&mut self, editor: &mut Editor<RadishRustylineHelper>) -> ReplResult<()> {
        let line = editor.readline(self.prompt);

        match line {
            Ok(line) if line == HELP => {
                self.print_help();
                editor.add_history_entry(line.to_string());
                ReplResult::Ok(())
            }
            Ok(line) => {
                self.lines.push(line.clone());
                editor.add_history_entry(line.to_string());

                match self.eval() {
                    Ok(_) => ReplResult::Ok(()),
                    Err(err) => match err {
                        RadishError::CompilerError(err) => {
                            // check if the error was caused by an unexpected end of input.
                            // if it was then we persume that the user meant to keep typing.
                            if err.clone().is_unexpected_eof() {
                                self.prompt = CONTINUE;

                                self.readline(editor)
                            } else {
                                // otherwise turn it back into a RadishError and report it.
                                ReplResult::Error(err.into())
                            }
                        }
                        _ => ReplResult::Error(err),
                    },
                }
            }
            Err(ReadlineError::Interrupted) => ReplResult::Interrupted,
            Err(ReadlineError::Eof) => ReplResult::Eof,
            Err(err) => ReplResult::Error(err.to_string().into()),
        }
    }

    /// The evil, err, _eval_ part of REPL.
    fn eval(&mut self) -> Result<(), RadishError> {
        let result = self.vm.eval(&self.lines.join("\n"))?;

        println!("{:?}", result);

        Ok(())
    }

    /// Reset the REPL's state.
    fn reset(&mut self) {
        //self.lines.clear();
        self.prev_len = self.lines.len();
        self.prompt = PROMPT;
    }

    /// Print out a help message.
    fn print_help(&mut self) {
        let help_message = "Press Ctrl+C to abort current expression, Ctrl+D to exit the REPL";

        println!("{}", help_message);
    }

    // TODO: a bit much? maybe scale it down a little?
    /// Print out Radish's welcome message.
    fn print_welcome(&mut self) {
        let (term_width, _) = term_size::dimensions().unwrap();

        let max_msg_width = 93;

        let welcome = "Welcome to Radish!";
        let help = format!("type '{}' for more information", HELP);
        let exit = "press Ctrl+D to exit";
        let version = format!("Version {}", env!("CARGO_PKG_VERSION"));

        let lines: Vec<&str> = WELCOME.split("\n").collect();

        // if the terminal window is too small, print out a more compact
        // version, putting the welcome message, help information and such
        // under the banner instead of to the side.
        if term_width < max_msg_width {
            println!("{}\n", WELCOME);
            println!("{} ({})", welcome, version);
            println!("{}, {}", help, exit);
        } else {
            println!("{}", lines[1]);
            println!("{}{}", lines[2], welcome);
            println!("{}", lines[3]);
            println!("{}{}", lines[4], help);
            println!("{}{}", lines[5], exit);
            println!("{}", lines[6]);
            println!("{}{}", lines[7], version);
            println!("");
        }
    }
}

/// VM's welcome banner.
const WELCOME: &str = r#"
 ________  ________  ________  ___  ________  ___  ___       
|\   __  \|\   __  \|\   ___ \|\  \|\   ____\|\  \|\  \      
\ \  \|\  \ \  \|\  \ \  \_|\ \ \  \ \  \___|\ \  \\\  \     
 \ \   _  _\ \   __  \ \  \ \\ \ \  \ \_____  \ \   __  \    
  \ \  \\  \\ \  \ \  \ \  \_\\ \ \  \|____|\  \ \  \ \  \   
   \ \__\\ _\\ \__\ \__\ \_______\ \__\_________\ \__\ \__\  
    \|__|\|__|\|__|\|__|\|_______|\|__||_________||__|\|__|  "#;
