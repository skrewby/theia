use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Program(Vec<Statement>),
    Var(VarStatement),
    Return(Expression),
    Expression(Expression),
    Block(Vec<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    Int(i64),
    Float(f64),
    Str(String),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(bool),
    If(IfExpression),
    Function(FunctionExpression),
    Call(CallExpression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarStatement {
    pub identifier: Token,
    pub expression: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: Box<Statement>,
    pub alternative: Option<Box<Statement>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionExpression {
    pub parameters: Vec<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}
