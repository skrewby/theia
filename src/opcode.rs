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

            0x40 => Ok(Opcode::Pop),
            0x41 => Ok(Opcode::PushTrue),
            0x42 => Ok(Opcode::PushFalse),
            0x43 => Ok(Opcode::PushConstant),

            _ => Err(format!("Unknown opcode: 0x{:02X}", byte)),
        }
    }
}
