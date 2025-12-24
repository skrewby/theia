use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Program(Vec<Statement>),
    VariableAssign(LetStatement),
    Return(Expression),
    Break(Expression),
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
    While(WhileExpression),
    Function(FunctionExpression),
    Call(CallExpression),
    Array(Vec<Expression>),
    Index(IndexExpression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
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
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
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

#[derive(Debug, PartialEq, Clone)]
pub struct WhileExpression {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
}
