#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    /// Do nothing
    Nop = 0x00,

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
    /// 0x60 aa bb
    PushConstant = 0x44,

    // ------------ Jumps ------------ //
    Jump = 0x60,
    JumpNotTrue = 0x61,
}

impl Opcode {
    pub fn from_byte(byte: u8) -> Result<Self, String> {
        let opcode = match byte {
            0x00 => Opcode::Nop,

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

            _ => return Err(format!("Unknown opcode: 0x{:02X}", byte)),
        };

        Ok(opcode)
    }

    pub fn num_operands(&self) -> usize {
        match self {
            Opcode::PushConstant => 2,
            Opcode::Jump => 2,
            Opcode::JumpNotTrue => 2,
            _ => 0,
        }
    }
}
