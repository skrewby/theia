#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    // ---------- Arithmetic ---------- //
    /// Pop the top two values of the stack and push addition result
    Add = 0x20,

    // ---------- Stack ---------- //
    /// Pops the top value of the stack
    Pop = 0x30,

    // ---------- Constants ---------- //
    /// Push constant at index 0xaabb in the constant array to the stack
    /// 0x50 aa bb
    ConstantPush = 0x50,
}

impl Opcode {
    pub fn from_byte(byte: u8) -> Result<Self, String> {
        match byte {
            0x20 => Ok(Opcode::Add),
            0x30 => Ok(Opcode::Pop),
            0x50 => Ok(Opcode::ConstantPush),
            _ => Err(format!("Unknown opcode: 0x{:02X}", byte)),
        }
    }
}
