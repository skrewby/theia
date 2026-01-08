#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
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

    // ---------- Stack ---------- //
    /// Pops the top value of the stack
    Pop = 0x40,
    /// Pushes the boolean true to the stack
    PushTrue = 0x41,
    /// Pushes the boolean false to the stack
    PushFalse = 0x42,
    /// Push constant at index 0xaabb in the constant array to the stack
    /// 0x60 aa bb
    PushConstant = 0x43,
}

impl Opcode {
    pub fn from_byte(byte: u8) -> Result<Self, String> {
        match byte {
            0x20 => Ok(Opcode::Add),
            0x21 => Ok(Opcode::Sub),
            0x22 => Ok(Opcode::Mul),
            0x23 => Ok(Opcode::Div),
            0x24 => Ok(Opcode::Equal),
            0x25 => Ok(Opcode::NotEqual),
            0x26 => Ok(Opcode::GreaterThan),
            0x27 => Ok(Opcode::LessThan),
            0x28 => Ok(Opcode::Bang),
            0x29 => Ok(Opcode::Negate),

            0x40 => Ok(Opcode::Pop),
            0x41 => Ok(Opcode::PushTrue),
            0x42 => Ok(Opcode::PushFalse),
            0x43 => Ok(Opcode::PushConstant),

            _ => Err(format!("Unknown opcode: 0x{:02X}", byte)),
        }
    }

    #[allow(dead_code)]
    pub fn num_operands(&self) -> usize {
        match self {
            Opcode::Add => 0,
            Opcode::Sub => 0,
            Opcode::Mul => 0,
            Opcode::Div => 0,
            Opcode::Equal => 0,
            Opcode::NotEqual => 0,
            Opcode::GreaterThan => 0,
            Opcode::LessThan => 0,
            Opcode::Bang => 0,
            Opcode::Negate => 0,
            Opcode::Pop => 0,
            Opcode::PushTrue => 0,
            Opcode::PushFalse => 0,
            Opcode::PushConstant => 2,
        }
    }
}
