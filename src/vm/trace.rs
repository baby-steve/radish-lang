use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Trace {
    pub message: String,
    pub calls: Vec<String>, // TODO: different data type.
}

impl Trace {
    pub fn new(message: impl ToString) -> Trace {
        Trace {
            message: message.to_string(),
            calls: vec![],
        }
    }

    pub fn add_context(&mut self, ctx: String) {
        self.calls.push(ctx);
    }
}

impl Display for Trace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.message)

        // TODO: print backtrace
        //for call in self.calls.iter() {
        //    writeln!(f, "    In {}", call.to_string())?;
        //};

        //write!(f, "")
    }
}
