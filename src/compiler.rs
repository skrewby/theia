use crate::ast::{Expression, InfixExpression};
use crate::opcode::Opcode;
use crate::token::TokenType;
use crate::{ast::Statement, object::Object};

#[derive(Debug, PartialEq)]
pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Object>,
}

struct CompilerState {
    instructions: Vec<u8>,
    constants: Vec<Object>,
    errors: Vec<String>,
}

impl CompilerState {
    fn new() -> CompilerState {
        CompilerState {
            instructions: Vec::new(),
            constants: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    fn emit(&mut self, op: Opcode, operands: &[&[u8]]) {
        self.instructions.push(op as u8);
        for operand in operands {
            self.instructions.extend_from_slice(operand);
        }
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1) as u16
    }
}

pub fn compile(statement: &Statement) -> Result<Bytecode, Vec<String>> {
    let mut state = CompilerState::new();

    compile_statement(&mut state, statement);

    if state.has_errors() {
        return Err(state.errors);
    }

    Ok(Bytecode {
        instructions: state.instructions,
        constants: state.constants,
    })
}

fn compile_statement(state: &mut CompilerState, statement: &Statement) {
    match statement {
        Statement::Program(statements) => {
            for stmt in statements {
                compile_statement(state, stmt);
            }
        }
        Statement::Expression(expression) => compile_expression(state, expression),
        _ => {
            state.add_error(format!("Unsupported statement: {:?}", statement));
        }
    }
}

fn compile_expression(state: &mut CompilerState, expression: &Expression) {
    match expression {
        Expression::Infix(infix) => compile_infix_expression(state, infix),
        Expression::Int(val) => create_integer(state, *val),
        _ => {
            state.add_error(format!("Unsupported expression: {:?}", expression));
        }
    }
}

fn compile_infix_expression(state: &mut CompilerState, infix: &InfixExpression) {
    compile_expression(state, &infix.left);
    compile_expression(state, &infix.right);

    match infix.operator.token_type {
        TokenType::Plus => state.emit(Opcode::Add, &[]),
        _ => {
            state.add_error(format!("Unsupported infix operator: {:?}", infix.operator));
        }
    }
}

fn create_integer(state: &mut CompilerState, val: i64) {
    let const_idx = state.add_constant(Object::Int(val));
    state.emit(Opcode::ConstantPush, &[&const_idx.to_be_bytes()]);
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex_input, parser::parse_program};

    use super::*;

    #[test]
    fn integer_arithmetic() {
        let input = "
            10 + 20
        ";
        let expected = vec![
            // Push constants[0]
            Opcode::ConstantPush as u8,
            0x00,
            0x00,
            // Push constants[1]
            Opcode::ConstantPush as u8,
            0x00,
            0x01,
            Opcode::Add as u8,
        ];

        check_bytecode_match(input, &expected);
    }

    fn check_bytecode_match(input: &str, expected: &Vec<u8>) {
        let tokens = lex_input(input);
        let ast = parse_program(tokens).expect("Parse error");
        let bytecode = match compile(&ast) {
            Ok(bc) => bc,
            Err(errors) => {
                for error in &errors {
                    eprintln!("Compilation error: {}", error);
                }
                panic!("Compilation failed with {} errors", errors.len());
            }
        };

        assert_eq!(bytecode.instructions, *expected, "Bytecode mismatch");
    }
}
