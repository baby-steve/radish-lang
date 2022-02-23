/// A Radish's bytecode instruction set.
/// 
// Comments have the following format: a discription of the behavior of the instruction,
// its operands (if any) and its stack effect.  
#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    /// Load a `[Value]` from the constant pool unto the stack with up to a u8 index
    ///
    /// operands: u8 (index)
    ///
    /// -> [Value]
    LoadConst,
    /// Load a `[Value]` from the constant pool unto the stack with up to a u32 index
    ///
    /// operands: u32 (index)
    ///
    /// -> [Value]
    LoadConstLong,
    /// Remove the top `[Value]` off the stack.
    ///
    /// [Value] ->
    Del,
    // TODO: do we still need this?
    DefGlobal,
    /// Read `index` from the bytecode stream. Get the `[Value]` at variables[index] and
    /// push it onto the stack.
    ///
    /// operand: u32 (index)
    /// 
    /// -> [Value]
    LoadGlobal,
    /// Read `index` from the bytecode stream. Set the `[Value]` at variables[index] to the top
    /// `[Value]` on the stack.
    /// 
    /// operand: u32 (index)
    /// 
    /// [no change]
    SaveGlobal,
    /// Read `index` from bytecode stream. Get the `[Value]` at stack[index] and
    /// push it onto the stack.
    ///
    /// operand: u32 (index)
    ///
    /// -> [Value]
    LoadLocal,
    /// Read `index` from bytecode stream. Set the `[Value]` at stack[index] to the top
    /// `[Value]` on the stack.
    ///
    /// operand: u32 (index)
    ///
    /// [no change]
    SaveLocal,
    /// **UNIMPLEMENTED**
    GetCapture,
    /// **UNIMPLEMENTED**
    SetCapture,
    /// Push `[Value::Boolean(true)]` onto the stack.
    ///
    /// -> [Value::Boolean(true)]
    True,
    /// Push `[Value::Boolean(false)]` onto the stack.
    ///
    /// -> [Value::Boolean(false)]
    False,
    /// Push `[Value::Nil]` onto the stack.
    ///
    /// -> [Value::Nil]
    Nil,
    /// Remove the top two `[Value]`s from the stack, add them and push the result
    /// onto the stack.
    ///
    /// [Value][Value] -> [Value]
    Add,
    /// Remove the top two `[Value]`s from the stack, subtract them and push the result
    /// onto the stack.
    ///
    /// [Value][Value] -> [Value]
    Sub,
    /// Remove the top two `[Value]`s from the stack, multiple them and push the result
    /// onto the stack.
    ///
    /// [Value][Value] -> [Value]
    Mul,
    /// Remove the top two `[Value]`s from the stack, divide them and push the result
    /// onto the stack.
    ///
    /// [Value][Value] -> [Value]
    Div,
    /// Remove the top two `[Value]`s from the stack, divide them and push the resulting
    /// remainder onto the stack.
    ///
    /// [Value][Value] -> [Value]
    Rem,
    /// Remove the top two `[Value]`s, and determine if the first is less than the second.
    /// If it is push `[Value::Boolean(true)]` onto the stack. Otherwise push `[Value::Boolean(false)]`.
    ///
    /// [Value][Value] -> [Value::Boolean]
    CmpLT,
    /// Remove the top two `[Value]`s, and determine if the first is less than or equal to the the second.
    /// If it is push `[Value::Boolean(true)]` onto the stack. Otherwise push `[Value::Boolean(false)]`.
    ///
    /// [Value][Value] -> [Value::Boolean]
    CmpLTEq,
    /// Remove the top two `[Value]`s, and determine if the first is greater than the second.
    /// If it is push `[Value::Boolean(true)]` onto the stack. Otherwise push `[Value::Boolean(false)]`.
    ///
    /// [Value][Value] -> [Value::Boolean]
    CmpGT,
    /// Remove the top two `[Value]`s, and determine if the first is greater than or equal to the second.
    /// If it is push `[Value::Boolean(true)]` onto the stack. Otherwise push `[Value::Boolean(false)]`.
    ///
    /// [Value][Value] -> [Value::Boolean]
    CmpGTEq,
    /// Remove the top two `[Value]`s, and determine if the first is equal to the second.
    /// If it is push `[Value::Boolean(true)]` onto the stack. Otherwise push `[Value::Boolean(false)]`.
    ///
    /// [Value][Value] -> [Value::Boolean]
    CmpEq,
    /// Remove the top two `[Value]`s, and determine if the first is not equal to the second.
    /// If it is push `[Value::Boolean(true)]` onto the stack. Otherwise push `[Value::Boolean(false)]`.
    ///
    /// [Value][Value] -> [Value::Boolean]
    CmpNotEq,
    /// Negate the top `[Value]` on the stack.
    ///
    /// [Value] -> [-Value]
    Neg,
    /// Remove top `[Value]` from the stack. Push `[Value::Boolean(false)]` if the value is false. if the value
    /// is true push `[Value::Boolean(true)]`.
    ///
    /// [Value] -> [!Value]
    Not,
    /// Increase the instruction pointer by the given offset if the `[Value]` on the top of the stack
    /// is false
    ///
    /// operand: u16 (jump offset)
    ///
    /// [no change]
    JumpIfFalse,
    /// Increase the instruction pointer by the given offset if the `[Value]` on the top of the stack
    /// is true
    ///
    /// operand: u16 (jump offset)
    ///
    /// [no change]
    JumpIfTrue,
    /// Unconditionally _increase_ instruction pointer by the given offset.
    ///
    /// operand: u16 (jump offset)
    ///
    /// [no change]
    Jump,
    /// Unconditionally _decrease_ instruction pointer by the given offset.
    ///
    /// operand: u16 (loop offset)
    ///
    /// [no change]
    Loop,
    /// Read `arg_count` from the top of the stack then peek `arg_count` places into the
    /// stack and call the `[Value]` that it finds, pushing a new call frame onto the callstack.
    ///
    /// operands: u8 (argument count)
    ///
    /// [function][arg_1][arg_2]...[arg_n]
    Call,
    /// Remove the top `[Value]` from the stack and construct a closure from it.
    ///
    /// [Value::Function] -> [Value::Closure]
    Closure,
    /// TODO:
    Class,
    /// Remove the top `[Value]` from the stack and write it to standard out.
    ///
    /// **Note:** this is a temporary instruction and will eventually be replaced by
    /// standard library functions.
    ///
    /// [Value] ->
    Print,
    /// Remove the top `[Value]` from the stack and return from the current call frame, before
    /// push the removed value back unto the stack.
    ///
    /// [Value::Function][Value][return value] -> [return value]
    Return,
}

impl From<u8> for Opcode {
    /// convert a raw byte into an opcode.
    /// Note that non-opcode bytes should never be interpreted as opcodes (will break).
    fn from(op: u8) -> Opcode {
        unsafe { std::mem::transmute(op) }
    }
}
