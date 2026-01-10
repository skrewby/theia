use crate::ast::{
    CallExpression, Expression, FunctionExpression, IfExpression, IndexExpression, InfixExpression,
    LetStatement, PrefixExpression,
};
use crate::compiler::symbol_table::{SymbolScope, SymbolTable};
use crate::object::FunctionObject;
use crate::opcode::Opcode;
use crate::token::TokenType;
use crate::{ast::Statement, object::Object};

pub mod symbol_table;

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

#[derive(Debug)]
struct Scope {
    instructions: Vec<u8>,
    latest_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            instructions: Vec::new(),
            latest_instruction: EmittedInstruction::default(),
            previous_instruction: EmittedInstruction::default(),
        }
    }
}

struct CompilerState {
    constants: Vec<Object>,
    errors: Vec<String>,

    scopes: Vec<Scope>,
    scope_index: usize,
    symbol_table: SymbolTable,
}

impl CompilerState {
    fn new() -> CompilerState {
        let mut scopes = Vec::new();
        scopes.push(Scope::default());

        CompilerState {
            constants: Vec::new(),
            errors: Vec::new(),
            symbol_table: SymbolTable::new(),
            scopes,
            scope_index: 0,
        }
    }

    fn new_with_state(symbol_table: SymbolTable, constants: Vec<Object>) -> CompilerState {
        let mut scopes = Vec::new();
        scopes.push(Scope::default());

        CompilerState {
            constants,
            errors: Vec::new(),
            symbol_table,
            scopes,
            scope_index: 0,
        }
    }

    fn add_error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    fn instructions(&self) -> &Vec<u8> {
        &self.scopes[self.scope_index].instructions
    }

    fn push_instruction(&mut self, opcode: Opcode) {
        self.scopes[self.scope_index]
            .instructions
            .push(opcode as u8);
    }

    fn update_previous_instruction(&mut self) {
        self.scopes[self.scope_index].previous_instruction =
            self.scopes[self.scope_index].latest_instruction;
    }

    fn latest_instruction(&self) -> EmittedInstruction {
        self.scopes[self.scope_index].latest_instruction
    }

    fn truncate_instruction(&mut self, position: usize) {
        self.scopes[self.scope_index]
            .instructions
            .truncate(position);
    }

    fn update_latest_instruction(&mut self) {
        self.scopes[self.scope_index].latest_instruction =
            self.scopes[self.scope_index].previous_instruction;
    }

    fn set_latest_instruction(&mut self, opcode: Opcode, position: u16) {
        self.scopes[self.scope_index].latest_instruction = EmittedInstruction { opcode, position };
    }

    fn replace_latest_with(&mut self, opcode: Opcode) {
        let pos = self.scopes[self.scope_index].latest_instruction.position;
        let data = (opcode as u8).to_be_bytes();
        self.patch(pos, &data);
        self.scopes[self.scope_index].latest_instruction.opcode = opcode;
    }

    fn emit(&mut self, op: Opcode, operands: &[&[u8]]) {
        let position = self.get_current_position();
        self.update_previous_instruction();
        self.push_instruction(op);
        self.set_latest_instruction(op, position);

        for operand in operands {
            self.scopes[self.scope_index]
                .instructions
                .extend_from_slice(operand);
        }
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1) as u16
    }

    fn remove_latest_instruction(&mut self) {
        self.truncate_instruction(self.latest_instruction().position as usize);
        self.update_latest_instruction();
    }

    fn patch(&mut self, start_pos: u16, data: &[u8]) {
        let p = start_pos as usize;
        self.scopes[self.scope_index].instructions[p..p + data.len()].copy_from_slice(data);
    }

    fn get_current_position(&self) -> u16 {
        self.scopes[self.scope_index].instructions.len() as u16
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
        self.scope_index += 1;

        let outer = std::mem::replace(&mut self.symbol_table, SymbolTable::new());
        self.symbol_table = SymbolTable::new_enclosed(outer);
    }

    fn leave_scope(&mut self) -> Vec<u8> {
        let instructions = self.instructions().clone();

        self.scope_index -= 1;
        self.scopes.truncate(self.scope_index + 1);

        let inner = std::mem::replace(&mut self.symbol_table, SymbolTable::new());
        self.symbol_table = inner.into_outer();

        instructions
    }

    fn is_latest(&mut self, opcode: Opcode) -> bool {
        if self.instructions().len() == 0 {
            return false;
        }

        self.latest_instruction().opcode == opcode
    }
}

pub fn compile(statement: &Statement) -> Result<Bytecode, Vec<String>> {
    let mut state = CompilerState::new();

    compile_statement(&mut state, statement);

    if state.has_errors() {
        return Err(state.errors);
    }

    Ok(Bytecode {
        instructions: state.instructions().clone(),
        constants: state.constants,
    })
}

pub struct CompilationResult {
    pub bytecode: Bytecode,
    pub symbol_table: SymbolTable,
}

pub fn compile_with_state(
    statement: &Statement,
    symbol_table: SymbolTable,
    constants: Vec<Object>,
) -> Result<CompilationResult, Vec<String>> {
    let mut state = CompilerState::new_with_state(symbol_table, constants);

    compile_statement(&mut state, statement);

    if state.has_errors() {
        return Err(state.errors);
    }

    Ok(CompilationResult {
        bytecode: Bytecode {
            instructions: state.instructions().clone(),
            constants: state.constants,
        },
        symbol_table: state.symbol_table,
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
        Statement::VariableAssign(statement) => {
            compile_variable_assign(state, statement);
        }
        Statement::Return(expression) => {
            compile_expression(state, expression);
            state.emit(Opcode::ReturnValue, &[]);
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
        Expression::Int(val) => create_constant(state, Object::Int(*val)),
        Expression::Float(val) => create_constant(state, Object::Float(*val)),
        Expression::Str(val) => create_constant(state, Object::Str(val.to_owned())),
        Expression::Boolean(val) => push_boolean(state, *val),
        Expression::If(ex) => compile_if_expression(state, ex),
        Expression::Identifier(val) => compile_identifier_expression(state, val),
        Expression::Array(val) => compile_array(state, val),
        Expression::Index(index) => compile_index(state, index),
        Expression::Function(func) => compile_function(state, func),
        Expression::Call(call) => compile_call(state, call),
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

fn compile_identifier_expression(state: &mut CompilerState, name: &str) {
    let Some(symbol) = state.symbol_table.resolve(name) else {
        state.add_error(format!("Identifier not found: {:?}", name));
        return;
    };

    let opcode = match symbol.scope {
        SymbolScope::Global => Opcode::GetGlobal,
        SymbolScope::Local => Opcode::GetLocal,
    };
    state.emit(opcode, &[&symbol.index.to_be_bytes()]);
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
    if state.is_latest(Opcode::Pop) {
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
    if state.is_latest(Opcode::Pop) {
        state.remove_latest_instruction();
    }

    let after_alternative_pos = state.get_current_position();
    let data = make_bytecode(Opcode::Jump, &[&after_alternative_pos.to_be_bytes()]);
    state.patch(jump_pos, &data);

    alternative_pos
}

fn compile_function(state: &mut CompilerState, func: &FunctionExpression) {
    state.enter_scope();

    for par in &func.parameters {
        let Expression::Identifier(val) = par else {
            state.add_error(format!("Invalid parameter: {:?}", par));
            return;
        };
        state.symbol_table.define(&val);
    }

    compile_statement(state, &func.body);
    if state.is_latest(Opcode::Pop) {
        state.replace_latest_with(Opcode::ReturnValue);
    }
    if !state.is_latest(Opcode::ReturnValue) {
        state.emit(Opcode::Return, &[]);
    }

    let num_locals = state.symbol_table.num_definitions;
    let instructions = state.leave_scope();
    let obj = Object::Function(FunctionObject {
        instructions,
        num_locals,
        num_parameters: func.parameters.len(),
    });
    let const_idx = state.add_constant(obj);
    state.emit(Opcode::PushConstant, &[&const_idx.to_be_bytes()]);
}

fn create_constant(state: &mut CompilerState, obj: Object) {
    let const_idx = state.add_constant(obj);
    state.emit(Opcode::PushConstant, &[&const_idx.to_be_bytes()]);
}

fn push_boolean(state: &mut CompilerState, val: bool) {
    match val {
        true => state.emit(Opcode::PushTrue, &[]),
        false => state.emit(Opcode::PushFalse, &[]),
    }
}

fn compile_variable_assign(state: &mut CompilerState, statement: &LetStatement) {
    compile_expression(state, &statement.expression);

    let TokenType::Identifier(identifier) = &statement.identifier.token_type else {
        state.add_error(format!(
            "Value {:?} is not a valid binding name for a variable",
            statement.identifier
        ));
        return;
    };

    let symbol = state.symbol_table.define(identifier);
    let opcode = match symbol.scope {
        SymbolScope::Global => Opcode::SetGlobal,
        SymbolScope::Local => Opcode::SetLocal,
    };
    state.emit(opcode, &[&symbol.index.to_be_bytes()]);
}

fn compile_array(state: &mut CompilerState, expressions: &Vec<Expression>) {
    for exp in expressions {
        compile_expression(state, exp);
    }

    let array_len = expressions.len() as u16;
    state.emit(Opcode::Array, &[&array_len.to_be_bytes()]);
}

fn compile_index(state: &mut CompilerState, expression: &IndexExpression) {
    compile_expression(state, &expression.left);
    compile_expression(state, &expression.index);
    state.emit(Opcode::Index, &[]);
}

fn compile_call(state: &mut CompilerState, call: &CallExpression) {
    compile_expression(state, &call.function);

    for a in &call.args {
        compile_expression(state, a);
    }

    state.emit(Opcode::Call, &[&(call.args.len() as u8).to_be_bytes()]);
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lex_input, object::FunctionObject, parser::parse_program};

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

    #[test]
    fn global_variables() {
        let input = "
            let one = 1;
            let two = one;
            one;
        ";
        let expected = vec![
            Opcode::PushConstant as u8,
            0x00,
            0x00,
            Opcode::SetGlobal as u8,
            0x00,
            0x00,
            Opcode::GetGlobal as u8,
            0x00,
            0x00,
            Opcode::SetGlobal as u8,
            0x00,
            0x01,
            Opcode::GetGlobal as u8,
            0x00,
            0x00,
            Opcode::Pop as u8,
        ];
        let constants = vec![Object::Int(1)];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn strings() {
        let input = "
            \"theia\";
            \"hello\" + \"world\";
        ";
        let expected = vec![
            Opcode::PushConstant as u8,
            0x00,
            0x00,
            Opcode::Pop as u8,
            Opcode::PushConstant as u8,
            0x00,
            0x01,
            Opcode::PushConstant as u8,
            0x00,
            0x02,
            Opcode::Add as u8,
            Opcode::Pop as u8,
        ];
        let constants = vec![
            Object::Str("theia".to_owned()),
            Object::Str("hello".to_owned()),
            Object::Str("world".to_owned()),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn arrays() {
        let input = "
            [];
            [1, 2, 3 + 2][1];
        ";
        let expected = vec![
            Opcode::Array as u8,
            0x00,
            0x00,
            Opcode::Pop as u8,
            Opcode::PushConstant as u8,
            0x00,
            0x00,
            Opcode::PushConstant as u8,
            0x00,
            0x01,
            Opcode::PushConstant as u8,
            0x00,
            0x02,
            Opcode::PushConstant as u8,
            0x00,
            0x03,
            Opcode::Add as u8,
            Opcode::Array as u8,
            0x00,
            0x03,
            Opcode::PushConstant as u8,
            0x00,
            0x04,
            Opcode::Index as u8,
            Opcode::Pop as u8,
        ];
        let constants = vec![
            Object::Int(1),
            Object::Int(2),
            Object::Int(3),
            Object::Int(2),
            Object::Int(1),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn functions_explicit_return() {
        let input = "
            fn() { return 5 + 10 }
        ";
        let expected = vec![Opcode::PushConstant as u8, 0x00, 0x02, Opcode::Pop as u8];
        let constants = vec![
            Object::Int(5),
            Object::Int(10),
            Object::Function(FunctionObject {
                instructions: vec![
                    Opcode::PushConstant as u8,
                    0x00,
                    0x00,
                    Opcode::PushConstant as u8,
                    0x00,
                    0x01,
                    Opcode::Add as u8,
                    Opcode::ReturnValue as u8,
                ],
                num_locals: 0,
                num_parameters: 0,
            }),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn functions_implicit_return() {
        let input = "
            fn() { 5 + 10 }
        ";
        let expected = vec![Opcode::PushConstant as u8, 0x00, 0x02, Opcode::Pop as u8];
        let constants = vec![
            Object::Int(5),
            Object::Int(10),
            Object::Function(FunctionObject {
                instructions: vec![
                    Opcode::PushConstant as u8,
                    0x00,
                    0x00,
                    Opcode::PushConstant as u8,
                    0x00,
                    0x01,
                    Opcode::Add as u8,
                    Opcode::ReturnValue as u8,
                ],
                num_locals: 0,
                num_parameters: 0,
            }),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn functions_multiple_expressions() {
        let input = "
            fn() { 5; 10 }
        ";
        let expected = vec![Opcode::PushConstant as u8, 0x00, 0x02, Opcode::Pop as u8];
        let constants = vec![
            Object::Int(5),
            Object::Int(10),
            Object::Function(FunctionObject {
                instructions: vec![
                    Opcode::PushConstant as u8,
                    0x00,
                    0x00,
                    Opcode::Pop as u8,
                    Opcode::PushConstant as u8,
                    0x00,
                    0x01,
                    Opcode::ReturnValue as u8,
                ],
                num_locals: 0,
                num_parameters: 0,
            }),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn functions_no_return() {
        let input = "
            fn() { }
        ";
        let expected = vec![Opcode::PushConstant as u8, 0x00, 0x00, Opcode::Pop as u8];
        let constants = vec![Object::Function(FunctionObject {
            instructions: vec![Opcode::Return as u8],
            num_locals: 0,
            num_parameters: 0,
        })];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn function_calls_literal() {
        let input = "
            fn() { 40 }();
        ";
        let expected = vec![
            Opcode::PushConstant as u8,
            0x00,
            0x01,
            Opcode::Call as u8,
            0x00,
            Opcode::Pop as u8,
        ];
        let constants = vec![
            Object::Int(40),
            Object::Function(FunctionObject {
                instructions: vec![
                    Opcode::PushConstant as u8,
                    0x00,
                    0x00,
                    Opcode::ReturnValue as u8,
                ],
                num_locals: 0,
                num_parameters: 0,
            }),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn function_calls_variable() {
        let input = "
            let x = fn() { 40 };
            x();
        ";
        let expected = vec![
            Opcode::PushConstant as u8,
            0x00,
            0x01,
            Opcode::SetGlobal as u8,
            0x00,
            0x00,
            Opcode::GetGlobal as u8,
            0x00,
            0x00,
            Opcode::Call as u8,
            0x00,
            Opcode::Pop as u8,
        ];
        let constants = vec![
            Object::Int(40),
            Object::Function(FunctionObject {
                instructions: vec![
                    Opcode::PushConstant as u8,
                    0x00,
                    0x00,
                    Opcode::ReturnValue as u8,
                ],
                num_locals: 0,
                num_parameters: 0,
            }),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn function_calls_arguments() {
        let input = "
            let x = fn(a, b) { a + b };
            x(2, 3);
        ";
        let expected = vec![
            Opcode::PushConstant as u8,
            0x00,
            0x00,
            Opcode::SetGlobal as u8,
            0x00,
            0x00,
            Opcode::GetGlobal as u8,
            0x00,
            0x00,
            Opcode::PushConstant as u8,
            0x00,
            0x01,
            Opcode::PushConstant as u8,
            0x00,
            0x02,
            Opcode::Call as u8,
            0x02,
            Opcode::Pop as u8,
        ];
        let constants = vec![
            Object::Function(FunctionObject {
                instructions: vec![
                    Opcode::GetLocal as u8,
                    0x00,
                    0x00,
                    Opcode::GetLocal as u8,
                    0x00,
                    0x01,
                    Opcode::Add as u8,
                    Opcode::ReturnValue as u8,
                ],
                num_locals: 0,
                num_parameters: 0,
            }),
            Object::Int(2),
            Object::Int(3),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn function_local_scope() {
        let input = "
            let y = 15;
            fn() {
                let x = 5;
                x + y
            }
        ";
        let expected = vec![
            Opcode::PushConstant as u8,
            0x00,
            0x00,
            Opcode::SetGlobal as u8,
            0x00,
            0x00,
            Opcode::PushConstant as u8,
            0x00,
            0x02,
            Opcode::Pop as u8,
        ];
        let constants = vec![
            Object::Int(15),
            Object::Int(5),
            Object::Function(FunctionObject {
                instructions: vec![
                    Opcode::PushConstant as u8,
                    0x00,
                    0x01,
                    Opcode::SetLocal as u8,
                    0x00,
                    0x00,
                    Opcode::GetLocal as u8,
                    0x00,
                    0x00,
                    Opcode::GetGlobal as u8,
                    0x00,
                    0x00,
                    Opcode::Add as u8,
                    Opcode::ReturnValue as u8,
                ],
                num_locals: 0,
                num_parameters: 0,
            }),
        ];

        check_bytecode_match(input, &expected, &constants);
    }

    #[test]
    fn scopes() {
        let mut compiler = CompilerState::new();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(Opcode::Mul, &[]);
        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);

        compiler.emit(Opcode::Sub, &[]);
        assert_eq!(compiler.instructions().len(), 1);

        compiler.leave_scope();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(Opcode::Add, &[]);
        assert_eq!(compiler.instructions().len(), 2);
        assert_eq!(compiler.latest_instruction().opcode, Opcode::Add);
        assert_eq!(
            compiler.scopes[compiler.scope_index]
                .previous_instruction
                .opcode,
            Opcode::Mul
        );
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

        pretty_print_instructions(&bytecode.instructions, expected);
        assert_eq!(bytecode.instructions, *expected, "Bytecode mismatch");
        assert_eq!(bytecode.constants, *constants, "Constants mismatch");
    }

    fn pretty_print_instructions(compiler: &Vec<u8>, expected: &Vec<u8>) {
        println!("{}", convert_bytes_to_string("Compiler", compiler));
        println!("{}", convert_bytes_to_string("Expected", expected));
    }

    fn convert_bytes_to_string(name: &str, instructions: &Vec<u8>) -> String {
        let mut result = String::with_capacity(instructions.len());
        result.push_str(name);
        result.push_str(": ");

        let mut operands_left = 0;
        for byte in instructions {
            if operands_left > 0 {
                result.push_str(&format!(" {:#02x} ", byte));
                operands_left -= 1;
                continue;
            }

            let opcode = Opcode::from_byte(*byte).unwrap();
            operands_left = opcode.num_operands();
            result.push_str(&format!(" {:?} ", opcode));
        }

        result
    }
}
