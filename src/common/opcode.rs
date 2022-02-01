#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    LoadConst,
    LoadConstLong,
    Pop,   
    
    DefGlobal,
    GetGlobal,
    SetGlobal,

    GetLocal,
    SetLocal,

    True,
    False,
    Nil,
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    EqualsTo,
    NotEqual,

    Negate,
    Not,

    JumpIfFalse,
    JumpIfTrue,
    Jump,
    Loop,

    Call,

    Print,
    Return,
}

impl From<u8> for Opcode {
    /// convert a raw byte into an opcode.
    /// Note that non-opcode bytes should never be interpreted as opcodes (will break).
    fn from(op: u8) -> Opcode {
        unsafe { std::mem::transmute(op) }
    }
}