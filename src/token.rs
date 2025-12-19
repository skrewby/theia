#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers and literals
    Identifier(String),
    Int(u64),
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

/// This is a copy of TokenType, however there will be no data associated with these values.
/// This is so these values can be used for things like HashMap keys
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenKind {
    Illegal,
    EOF,

    // Identifiers and literals
    Identifier,
    Int,
    Float,

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

impl From<&TokenType> for TokenKind {
    fn from(token_type: &TokenType) -> Self {
        match token_type {
            TokenType::Illegal => TokenKind::Illegal,
            TokenType::EOF => TokenKind::EOF,
            TokenType::Identifier(_) => TokenKind::Identifier,
            TokenType::Int(_) => TokenKind::Int,
            TokenType::Float(_) => TokenKind::Float,
            TokenType::Assign => TokenKind::Assign,
            TokenType::Plus => TokenKind::Plus,
            TokenType::Minus => TokenKind::Minus,
            TokenType::Bang => TokenKind::Bang,
            TokenType::Asterisk => TokenKind::Asterisk,
            TokenType::Slash => TokenKind::Slash,
            TokenType::LessThan => TokenKind::LessThan,
            TokenType::GreaterThan => TokenKind::GreaterThan,
            TokenType::Equal => TokenKind::Equal,
            TokenType::NotEqual => TokenKind::NotEqual,
            TokenType::Comma => TokenKind::Comma,
            TokenType::Semicolon => TokenKind::Semicolon,
            TokenType::LParen => TokenKind::LParen,
            TokenType::RParen => TokenKind::RParen,
            TokenType::LBrace => TokenKind::LBrace,
            TokenType::RBrace => TokenKind::RBrace,
            TokenType::Function => TokenKind::Function,
            TokenType::Variable => TokenKind::Variable,
            TokenType::True => TokenKind::True,
            TokenType::False => TokenKind::False,
            TokenType::If => TokenKind::If,
            TokenType::Else => TokenKind::Else,
            TokenType::Return => TokenKind::Return,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
}
