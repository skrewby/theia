#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers and literals
    Identifier(String),
    Int(i64),
    Float(f64),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Variable,
    True,
    False,
    If,
    Else,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
}
