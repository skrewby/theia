#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
/// Opcodes used with the Theia Virtual Machine
/// This is a big endian architecture that operates multiples of bytes.
///
/// An opcode that has two operands will consist of the following
/// slice in memory: [opcode,byte,byte]
pub enum Opcode {
    /// Do nothing
    Nop = 0x00,
    /// Pop the top value of the stack and calls it
    Call = 0x01,
    /// Return from the function
    Return = 0x02,
    /// Return from the function and push returned value
    ReturnValue = 0x03,

    // ---------- Operators ---------- //
    /// Pop the top two values of the stack and push addition result
    Add = 0x20,
    /// Pop the top two values of the stack and push subtraction result
    Sub = 0x21,
    /// Pop the top two values of the stack and push multiplication result
    Mul = 0x22,
    /// Pop the top two values of the stack and push division result
    Div = 0x23,
    /// Pop the top two values of the stack and push equality result
    Equal = 0x24,
    /// Pop the top two values of the stack and push in-equality result
    NotEqual = 0x25,
    /// Pop the top two values of the stack and push greater than result
    GreaterThan = 0x26,
    /// Pop the top two values of the stack and push less than result
    LessThan = 0x27,
    /// Pop the top of the stack and push the !value
    Bang = 0x28,
    /// Pop the top of the stack and push the negate result
    Negate = 0x29,

    // ------------ Stack ------------ //
    /// Pops the top value of the stack
    Pop = 0x40,
    /// Pushes the boolean true to the stack
    PushTrue = 0x41,
    /// Pushes the boolean false to the stack
    PushFalse = 0x42,
    /// Pushes null to the stack
    PushNull = 0x43,
    /// Push constant at index 0xaabb in the constant array to the stack
    /// 0x44 aa bb
    PushConstant = 0x44,

    // ------------ Jumps ------------ //
    Jump = 0x60,
    JumpNotTrue = 0x61,

    // ---------- Variables ---------- //
    /// Bind the top value on the stack to global variable num 0xnnnn
    /// 0x80 nn nn
    SetGlobal = 0x80,
    /// Push global variable num 0xnnnn to the stack
    /// 0x81 nn nn
    GetGlobal = 0x81,
    /// Bind the top value on the stack to local variable num 0xnnnn
    /// 0x82 nn nn
    SetLocal = 0x82,
    /// Push local variable num 0xnnnn to the stack
    /// 0x81 nn nn
    GetLocal = 0x83,

    // --------- Composites ---------- //
    // Pops nnnn elements from the stack then builds and push the array
    // 0x90 nn nn
    Array = 0x90,
    /// Pops two elements from the stack, the index number then the object to be indexed
    Index = 0x91,
}

impl Opcode {
    pub fn from_byte(byte: u8) -> Result<Self, String> {
        let opcode = match byte {
            0x00 => Opcode::Nop,
            0x01 => Opcode::Call,
            0x02 => Opcode::Return,
            0x03 => Opcode::ReturnValue,

            0x20 => Opcode::Add,
            0x21 => Opcode::Sub,
            0x22 => Opcode::Mul,
            0x23 => Opcode::Div,
            0x24 => Opcode::Equal,
            0x25 => Opcode::NotEqual,
            0x26 => Opcode::GreaterThan,
            0x27 => Opcode::LessThan,
            0x28 => Opcode::Bang,
            0x29 => Opcode::Negate,

            0x40 => Opcode::Pop,
            0x41 => Opcode::PushTrue,
            0x42 => Opcode::PushFalse,
            0x43 => Opcode::PushNull,
            0x44 => Opcode::PushConstant,

            0x60 => Opcode::Jump,
            0x61 => Opcode::JumpNotTrue,

            0x80 => Opcode::SetGlobal,
            0x81 => Opcode::GetGlobal,
            0x82 => Opcode::SetLocal,
            0x83 => Opcode::GetLocal,

            0x90 => Opcode::Array,
            0x91 => Opcode::Index,

            _ => return Err(format!("Unknown opcode: 0x{:02X}", byte)),
        };

        Ok(opcode)
    }

    pub fn num_operands(&self) -> usize {
        match self {
            Opcode::PushConstant => 2,
            Opcode::SetGlobal => 2,
            Opcode::GetGlobal => 2,
            Opcode::SetLocal => 2,
            Opcode::GetLocal => 2,
            Opcode::Jump => 2,
            Opcode::JumpNotTrue => 2,
            Opcode::Array => 2,
            Opcode::Call => 1,
            _ => 0,
        }
    }
}
