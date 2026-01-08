use crate::ast::{Expression, IfExpression, InfixExpression, PrefixExpression};
use crate::opcode::Opcode;
use crate::token::TokenType;
use crate::{ast::Statement, object::Object};

#[derive(Debug, PartialEq)]
pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Object>,
}

#[derive(Debug, Clone, Copy)]
struct EmittedInstruction {
    opcode: Opcode,
    position: u16,
}

impl Default for EmittedInstruction {
    fn default() -> Self {
        Self {
            opcode: Opcode::Nop,
            position: 0,
        }
    }
}

struct CompilerState {
    instructions: Vec<u8>,
    constants: Vec<Object>,
    errors: Vec<String>,

    latest_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

impl CompilerState {
    fn new() -> CompilerState {
        CompilerState {
            instructions: Vec::new(),
            constants: Vec::new(),
            errors: Vec::new(),
            latest_instruction: EmittedInstruction::default(),
            previous_instruction: EmittedInstruction::default(),
        }
    }

    fn add_error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    fn emit(&mut self, op: Opcode, operands: &[&[u8]]) {
        let position = self.get_current_position();
        self.previous_instruction = self.latest_instruction;
        self.instructions.push(op as u8);
        self.latest_instruction = EmittedInstruction {
            opcode: op,
            position,
        };

        for operand in operands {
            self.instructions.extend_from_slice(operand);
        }
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1) as u16
    }

    fn remove_latest_instruction(&mut self) {
        self.instructions
            .truncate(self.latest_instruction.position as usize);
        self.latest_instruction = self.previous_instruction;
    }

    fn patch(&mut self, start_pos: u16, data: &[u8]) {
        let p = start_pos as usize;
        self.instructions[p..p + data.len()].copy_from_slice(data);
    }

    fn get_current_position(&self) -> u16 {
        self.instructions.len() as u16
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

fn make_bytecode(op: Opcode, operands: &[&[u8]]) -> Vec<u8> {
    let mut instructions = Vec::new();

    instructions.push(op as u8);

    for operand in operands {
        instructions.extend_from_slice(operand);
    }

    instructions
}

fn compile_statement(state: &mut CompilerState, statement: &Statement) {
    match statement {
        Statement::Program(statements) | Statement::Block(statements) => {
            for stmt in statements {
                compile_statement(state, stmt);
            }
        }
        Statement::Expression(expression) => {
            compile_expression(state, expression);
            state.emit(Opcode::Pop, &[]);
        }
        _ => {
            state.add_error(format!("Unsupported statement: {:?}", statement));
        }
    }
}

fn compile_expression(state: &mut CompilerState, expression: &Expression) {
    match expression {
        Expression::Prefix(prefix) => compile_prefix_expression(state, prefix),
        Expression::Infix(infix) => compile_infix_expression(state, infix),
        Expression::Int(val) => create_integer(state, *val),
        Expression::Float(val) => create_float(state, *val),
        Expression::Boolean(val) => push_boolean(state, *val),
        Expression::If(ex) => compile_if_expression(state, ex),
        _ => {
            state.add_error(format!("Unsupported expression: {:?}", expression));
        }
    }
}

fn compile_prefix_expression(state: &mut CompilerState, prefix: &PrefixExpression) {
    compile_expression(state, &prefix.right);

    match prefix.operator.token_type {
        TokenType::Bang => state.emit(Opcode::Bang, &[]),
        TokenType::Minus => state.emit(Opcode::Negate, &[]),
        _ => {
            state.add_error(format!(
                "Unsupported prefix operator: {:?}",
                prefix.operator
            ));
        }
    }
}

fn compile_infix_expression(state: &mut CompilerState, infix: &InfixExpression) {
    compile_expression(state, &infix.left);
    compile_expression(state, &infix.right);

    match infix.operator.token_type {
        TokenType::Plus => state.emit(Opcode::Add, &[]),
        TokenType::Minus => state.emit(Opcode::Sub, &[]),
        TokenType::Asterisk => state.emit(Opcode::Mul, &[]),
        TokenType::Slash => state.emit(Opcode::Div, &[]),
        TokenType::Equal => state.emit(Opcode::Equal, &[]),
        TokenType::NotEqual => state.emit(Opcode::NotEqual, &[]),
        TokenType::GreaterThan => state.emit(Opcode::GreaterThan, &[]),
        TokenType::LessThan => state.emit(Opcode::LessThan, &[]),
        _ => {
            state.add_error(format!("Unsupported infix operator: {:?}", infix.operator));
        }
    }
}

fn compile_if_expression(state: &mut CompilerState, expression: &IfExpression) {
    compile_expression(state, &expression.condition);

    let jump_not_true_pos = state.get_current_position();
    state.emit(Opcode::JumpNotTrue, &[&[0xFF, 0xFF]]);

    compile_consequence(state, &expression.consequence);

    let jump_pos = state.get_current_position();
    state.emit(Opcode::Jump, &[&[0xFF, 0xFF]]);

    let after_pos = compile_alternative(state, &expression.alternative, jump_pos);

    let data = make_bytecode(Opcode::JumpNotTrue, &[&after_pos.to_be_bytes()]);
    state.patch(jump_not_true_pos, &data);
}

fn compile_consequence(state: &mut CompilerState, statement: &Box<Statement>) {
    compile_statement(state, statement);
    if state.latest_instruction.opcode == Opcode::Pop {
        state.remove_latest_instruction();
    }
}

fn compile_alternative(
    state: &mut CompilerState,
    statement: &Option<Box<Statement>>,
    jump_pos: u16,
) -> u16 {
    let alternative_pos = state.get_current_position();

    let Some(alternative) = statement else {
        state.emit(Opcode::PushNull, &[]);
        let data = make_bytecode(Opcode::Jump, &[&state.get_current_position().to_be_bytes()]);
        state.patch(jump_pos, &data);
        return alternative_pos;
    };

    compile_statement(state, alternative);
    if state.latest_instruction.opcode == Opcode::Pop {
        state.remove_latest_instruction();
    }

    let after_alternative_pos = state.get_current_position();
    let data = make_bytecode(Opcode::Jump, &[&after_alternative_pos.to_be_bytes()]);
    state.patch(jump_pos, &data);

    alternative_pos
}

fn create_integer(state: &mut CompilerState, val: i64) {
    let const_idx = state.add_constant(Object::Int(val));
    state.emit(Opcode::PushConstant, &[&const_idx.to_be_bytes()]);
}

fn create_float(state: &mut CompilerState, val: f64) {
    let const_idx = state.add_constant(Object::Float(val));
    state.emit(Opcode::PushConstant, &[&const_idx.to_be_bytes()]);
}

fn push_boolean(state: &mut CompilerState, val: bool) {
    match val {
        true => state.emit(Opcode::PushTrue, &[]),
        false => state.emit(Opcode::PushFalse, &[]),
    }
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
            Opcode::PushConstant as u8,
            0x00,
            0x00,
            Opcode::PushConstant as u8,
            0x00,
            0x01,
            Opcode::Add as u8,
            Opcode::Pop as u8,
        ];
        let constants = vec![Object::Int(10), Object::Int(20)];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn conditionals() {
        let input = "
            if true { 10 }; 100;
            if true { 20 } else { 30 }; 200;
        ";
        let expected = vec![
            Opcode::PushTrue as u8,
            Opcode::JumpNotTrue as u8,
            0x00,
            0x0A,
            Opcode::PushConstant as u8,
            0x00,
            0x00,
            Opcode::Jump as u8,
            0x00,
            0x0B,
            Opcode::PushNull as u8,
            Opcode::Pop as u8,
            Opcode::PushConstant as u8,
            0x00,
            0x01,
            Opcode::Pop as u8,
            Opcode::PushTrue as u8,
            Opcode::JumpNotTrue as u8,
            0x00,
            0x1A,
            Opcode::PushConstant as u8,
            0x00,
            0x02,
            Opcode::Jump as u8,
            0x00,
            0x1D,
            Opcode::PushConstant as u8,
            0x00,
            0x03,
            Opcode::Pop as u8,
            Opcode::PushConstant as u8,
            0x00,
            0x04,
            Opcode::Pop as u8,
        ];
        let constants = vec![
            Object::Int(10),
            Object::Int(100),
            Object::Int(20),
            Object::Int(30),
            Object::Int(200),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    fn check_bytecode_match(input: &str, expected: &Vec<u8>, constants: &Vec<Object>) {
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
        assert_eq!(bytecode.constants, *constants, "Constants mismatch");
    }
}
