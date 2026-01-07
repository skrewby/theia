use crate::{compiler::Bytecode, object::Object, opcode::Opcode};

const STACK_SIZE: usize = 2048;

pub trait ByteArrayExt {
    fn to_u8(&self) -> Result<u8, String>;
    fn to_u16(&self) -> Result<u16, String>;
}

impl ByteArrayExt for &[u8] {
    fn to_u8(&self) -> Result<u8, String> {
        if self.len() != 1 {
            return Err(format!("Expected 1 byte, got {}", self.len()));
        }
        Ok(self[0])
    }

    fn to_u16(&self) -> Result<u16, String> {
        if self.len() != 2 {
            return Err(format!("Expected 2 bytes, got {}", self.len()));
        }
        Ok(u16::from_be_bytes([self[0], self[1]]))
    }
}

pub struct VM {
    constants: Vec<Object>,
    instructions: Vec<u8>,
    stack: Vec<Object>,

    reg: Registers,
    last_popped: Option<Object>,
}

pub struct Registers {
    /// Stack pointer
    sp: usize,
    /// Instruction pointer
    ip: usize,
}

impl Registers {
    pub fn new() -> Registers {
        Registers { sp: 0, ip: 0 }
    }
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            reg: Registers::new(),
            last_popped: None,
        }
    }

    pub fn run(&mut self) -> Result<Object, String> {
        while self.reg.ip < self.instructions.len() {
            let opcode = self.fetch()?;
            self.decode_and_execute(&opcode)?;
        }

        Ok(self.stack_last_popped())
    }

    fn fetch(&mut self) -> Result<Opcode, String> {
        let opcode = Opcode::from_byte(self.instructions[self.reg.ip]);
        self.reg.ip += 1;

        opcode
    }

    fn decode_and_execute(&mut self, opcode: &Opcode) -> Result<(), String> {
        match opcode {
            Opcode::ConstantPush => op_constant_push(self)?,
            Opcode::Add => op_add(self)?,
            Opcode::Sub => op_sub(self)?,
            Opcode::Mul => op_mul(self)?,
            Opcode::Div => op_div(self)?,
            Opcode::Pop => {
                self.pop()?;
            }
        };

        Ok(())
    }

    fn push(&mut self, obj: Object) -> Result<(), String> {
        if self.reg.sp >= STACK_SIZE {
            return Err("Stack overflow".to_string());
        }

        if self.reg.sp >= self.stack.len() {
            self.stack.push(obj);
        } else {
            self.stack[self.reg.sp] = obj;
        }

        self.reg.sp += 1;
        Ok(())
    }

    fn pop(&mut self) -> Result<Object, String> {
        if self.reg.sp == 0 {
            return Err("Stack underflow".to_string());
        }

        self.reg.sp -= 1;
        let value = self.stack[self.reg.sp].clone();
        self.last_popped = Some(value.clone());
        Ok(value)
    }

    fn stack_last_popped(&self) -> Object {
        self.last_popped.clone().unwrap_or(Object::Null)
    }

    fn get_operands(&mut self, num: usize) -> &[u8] {
        let operands = &self.instructions[self.reg.ip..(self.reg.ip + num)];
        self.reg.ip += num;

        operands
    }

    fn get_constant(&self, idx: u16) -> Result<Object, String> {
        match self.constants.get(idx as usize) {
            Some(obj) => Ok(obj.clone()),
            None => Err(format!(
                "Unable to index the constant array at the requested value: {}",
                idx
            )),
        }
    }
}

fn op_constant_push(vm: &mut VM) -> Result<(), String> {
    let idx = vm.get_operands(2).to_u16()?;
    let constant = vm.get_constant(idx)?;
    vm.push(constant)?;
    Ok(())
}

fn op_add(vm: &mut VM) -> Result<(), String> {
    let right = vm.pop()?;
    let left = vm.pop()?;

    let result = eval_addition(&left, &right);
    vm.push(result)?;

    Ok(())
}

fn eval_addition(left: &Object, right: &Object) -> Object {
    match (&left, &right) {
        (Object::Int(_) | Object::Float(_), Object::Int(_) | Object::Float(_)) => {
            eval_sum_numbers(&left, &right, true)
        }
        _ if (matches!(&left, Object::Array(_)) || matches!(&right, Object::Array(_))) => {
            eval_concat_array(&left, &right)
        }
        _ if (matches!(&left, Object::Str(_)) || matches!(&right, Object::Str(_))) => {
            Object::Str(format!("{}{}", left.inspect(), right.inspect()))
        }
        _ => Object::Error(format!(
            "Type mismatch: {} + {}",
            left.inspect(),
            right.inspect()
        )),
    }
}

fn eval_sum_numbers(left: &Object, right: &Object, is_addition: bool) -> Object {
    match (left, right) {
        (Object::Int(l), Object::Int(r)) => Object::Int(if is_addition { l + r } else { l - r }),
        (Object::Float(l), Object::Float(r)) => {
            Object::Float(if is_addition { l + r } else { l - r })
        }
        (Object::Int(l), Object::Float(r)) => Object::Float(if is_addition {
            *l as f64 + r
        } else {
            *l as f64 - r
        }),
        (Object::Float(l), Object::Int(r)) => Object::Float(if is_addition {
            l + *r as f64
        } else {
            l - *r as f64
        }),
        _ => unreachable!(),
    }
}

fn eval_concat_array(left: &Object, right: &Object) -> Object {
    match (left, right) {
        (Object::Array(l), Object::Array(r)) => {
            let mut result = l.clone();
            result.extend(r.clone());
            Object::Array(result)
        }
        (Object::Array(l), r) => {
            let mut result = l.clone();
            result.push(r.clone());
            Object::Array(result)
        }
        (l, Object::Array(r)) => {
            let mut result = vec![l.clone()];
            result.extend(r.clone());
            Object::Array(result)
        }
        _ => unreachable!(),
    }
}

fn op_sub(vm: &mut VM) -> Result<(), String> {
    let right = vm.pop()?;
    let left = vm.pop()?;

    let result = match (&left, &right) {
        (Object::Int(_) | Object::Float(_), Object::Int(_) | Object::Float(_)) => {
            eval_sum_numbers(&left, &right, false)
        }
        _ => Object::Error(format!(
            "Type mismatch: {} - {}",
            left.inspect(),
            right.inspect()
        )),
    };

    vm.push(result)?;
    Ok(())
}

fn op_mul(vm: &mut VM) -> Result<(), String> {
    let right = vm.pop()?;
    let left = vm.pop()?;

    let result = match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Int(*l * *r),
        (Object::Float(l), Object::Float(r)) => Object::Float(*l * *r),
        (Object::Int(l), Object::Float(r)) => Object::Float(*l as f64 * *r),
        (Object::Float(l), Object::Int(r)) => Object::Float(*l * *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} * {}", left.inspect(), right.inspect()).to_owned(),
        ),
    };

    vm.push(result)?;
    Ok(())
}

fn op_div(vm: &mut VM) -> Result<(), String> {
    let right = vm.pop()?;
    let left = vm.pop()?;

    if right.is_zero() {
        let result = Object::Error(
            format!(
                "Unable to divide by zero: {} / {}",
                left.inspect(),
                right.inspect()
            )
            .to_owned(),
        );
        vm.push(result)?;
        return Ok(());
    };

    let result = match (&left, &right) {
        (Object::Int(l), Object::Int(r)) => Object::Int(*l / *r),
        (Object::Float(l), Object::Float(r)) => Object::Float(*l / *r),
        (Object::Int(l), Object::Float(r)) => Object::Float(*l as f64 / *r),
        (Object::Float(l), Object::Int(r)) => Object::Float(*l / *r as f64),
        _ => Object::Error(
            format!("Type mismatch: {} / {}", left.inspect(), right.inspect()).to_owned(),
        ),
    };
    vm.push(result)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{compiler::compile, lexer::lex_input, parser::parse_program};

    use super::*;

    impl VM {
        pub fn iter(self) -> VMIterator {
            VMIterator { vm: self }
        }

        fn step(&mut self) -> Result<Object, String> {
            while self.reg.ip < self.instructions.len() {
                let opcode = self.fetch()?;
                self.decode_and_execute(&opcode)?;

                if matches!(opcode, Opcode::Pop) {
                    return Ok(self.stack_last_popped());
                }
            }

            Err("No more instructions to execute".to_string())
        }
    }

    pub struct VMIterator {
        vm: VM,
    }

    impl Iterator for VMIterator {
        type Item = Result<Object, String>;

        fn next(&mut self) -> Option<Self::Item> {
            if self.vm.reg.ip >= self.vm.instructions.len() {
                return None;
            }

            Some(self.vm.step())
        }
    }

    #[test]
    fn constant_push() {
        let instructions = vec![Opcode::ConstantPush as u8, 0x00, 0x00, Opcode::Pop as u8];
        let constants = vec![Object::Int(42)];

        run_test_opcode_input(instructions, constants, Object::Int(42));
    }

    #[test]
    fn constant_push_multiple() {
        let instructions = vec![
            Opcode::ConstantPush as u8,
            0x00,
            0x00,
            Opcode::ConstantPush as u8,
            0x00,
            0x01,
            Opcode::Pop as u8,
        ];
        let constants = vec![Object::Int(10), Object::Int(20)];

        run_test_opcode_input(instructions, constants, Object::Int(20));
    }

    #[test]
    fn arithmetic() {
        let input = "
            10 + 20
            50 - 30
            30 * 4
            50 / 2
        ";
        let expected = vec![
            Object::Int(30),
            Object::Int(20),
            Object::Int(120),
            Object::Int(25),
        ];

        run_test(input, expected);
    }

    fn run_test(input: &str, expected: Vec<Object>) {
        let tokens = lex_input(input);
        let ast = match parse_program(tokens) {
            Ok(p) => p,
            Err(errors) => {
                for e in errors {
                    println!("{}", e);
                }
                panic!("Error when parsing program")
            }
        };

        let bytecode = match compile(&ast) {
            Ok(p) => p,
            Err(errors) => {
                for e in errors {
                    println!("{}", e);
                }
                panic!("error when compiling program")
            }
        };

        let vm = VM::new(bytecode);
        for (i, res) in vm.iter().enumerate() {
            let obj = match res {
                Ok(o) => o,
                Err(e) => {
                    panic!("Internal VM Error: {}", e);
                }
            };

            assert_eq!(obj, expected[i]);
        }
    }

    fn run_test_opcode_input(instructions: Vec<u8>, constants: Vec<Object>, expected: Object) {
        let mut vm = VM::new(Bytecode {
            instructions,
            constants,
        });
        let result = match vm.run() {
            Ok(res) => res,
            Err(err) => panic!("Internal VM error: {}", err),
        };

        assert_eq!(result, expected);
    }
}
