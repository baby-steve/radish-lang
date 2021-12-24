#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Constant,

    Add,
    Subtract,
    Multiply,
    Divide,

    Halt,
}

impl From<u8> for Opcode {
    /// convert a raw byte into an opcode.
    /// Note that non-opcode bytes should never be interpreted as opcodes (will break).
    fn from(op: u8) -> Opcode {
        unsafe { std::mem::transmute(op) }
    }
}