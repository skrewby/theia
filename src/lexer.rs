use std::borrow::Cow;

use crate::token::{Token, TokenType};

/// The lexer is responsible for converting our input into tokens which can then be consumed by our
/// parser
pub struct Lexer {
    /// Program input
    input: Vec<char>,
    /// Current position in input (points to current char)
    position: usize,
    /// Current reading position in input (points to after the current char)
    read_position: usize,
    /// The current char
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.pop_char();

        lexer
    }

    /// Consume the next character/s in the input and return their equivalent token
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token_type = if let Some(current_char) = self.ch {
            self.get_token_type(current_char)
        } else {
            TokenType::EOF
        };

        Token { token_type }
    }

    fn peek_char(&self) -> Option<char> {
        self.input.get(self.read_position).copied()
    }

    fn pop_char(&mut self) {
        self.ch = self.input.get(self.read_position).copied();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_some_and(|ch| ch.is_whitespace()) {
            self.pop_char();
        }
    }

    fn get_token_type(&mut self, ch: char) -> TokenType {
        let token = match ch {
            '=' => {
                if self.peek_char().is_some_and(|ch| ch == '=') {
                    self.pop_char();
                    TokenType::Equal
                } else {
                    TokenType::Assign
                }
            }
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '!' => {
                if self.peek_char().is_some_and(|ch| ch == '=') {
                    self.pop_char();
                    TokenType::NotEqual
                } else {
                    TokenType::Bang
                }
            }
            '/' => TokenType::Slash,
            '*' => TokenType::Asterisk,
            '<' => TokenType::LessThan,
            '>' => TokenType::GreaterThan,
            ',' => TokenType::Comma,
            ';' => TokenType::Semicolon,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            _ => {
                if is_letter(ch) {
                    let identifier = read_while(self, is_letter);
                    return get_identifier_token_type(&identifier);
                } else if is_digit(ch) {
                    let number = read_number(self);
                    return get_number_token_type(&number);
                } else {
                    self.pop_char();
                    return TokenType::Illegal;
                }
            }
        };

        self.pop_char();
        token
    }
}

fn read_while<F>(lexer: &mut Lexer, predicate: F) -> String
where
    F: Fn(char) -> bool,
{
    let pos = lexer.position;
    while lexer.ch.is_some_and(|ch| predicate(ch)) {
        lexer.pop_char();
    }

    lexer.input[pos..lexer.position].iter().collect()
}

fn read_number(lexer: &mut Lexer) -> String {
    let mut number = read_while(lexer, |ch| is_digit(ch) || ch == '_');

    // Hex - 0xABCD or 0XABCD or 0xabcd
    if number == "0" && lexer.ch.is_some_and(|ch| ch == 'x' || ch == 'X') {
        number.push(lexer.ch.unwrap());
        lexer.pop_char();
        number.push_str(&read_while(lexer, |ch| is_hex_digit(ch) || ch == '_'));
        return number;
    }

    // Float - 3.14
    if lexer.ch == Some('.') {
        number.push('.');
        lexer.pop_char();
        number.push_str(&read_while(lexer, |ch| is_digit(ch) || ch == '_'));
    }

    number
}

fn get_identifier_token_type(identifier: &str) -> TokenType {
    match identifier {
        "fn" => TokenType::Function,
        "var" => TokenType::Variable,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Identifier(identifier.to_owned()),
    }
}

fn get_number_token_type(number: &str) -> TokenType {
    let num: Cow<str> = if number.contains('_') {
        Cow::Owned(number.replace('_', ""))
    } else {
        Cow::Borrowed(number)
    };

    if num.starts_with("0x") || num.starts_with("0X") {
        i64::from_str_radix(&num[2..], 16).map_or(TokenType::Illegal, TokenType::Int)
    } else if num.contains('.') {
        num.parse::<f64>()
            .map_or(TokenType::Illegal, TokenType::Float)
    } else {
        num.parse::<i64>()
            .map_or(TokenType::Illegal, TokenType::Int)
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_hex_digit(ch: char) -> bool {
    ch.is_ascii_hexdigit()
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenType::*;

    #[test]
    fn simple_tokens() {
        let input = "=+-!*/(){},;<>===!==";
        let expected_types = vec![
            Assign,
            Plus,
            Minus,
            Bang,
            Asterisk,
            Slash,
            LParen,
            RParen,
            LBrace,
            RBrace,
            Comma,
            Semicolon,
            LessThan,
            GreaterThan,
            Equal,
            Assign,
            NotEqual,
            Assign,
        ];

        let mut lexer = Lexer::new(input);
        for expected in expected_types {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected);
        }
    }

    #[test]
    fn numbers() {
        let input = "5 10 100 1.23456 0xF 0xFFFF 10_000 0xFF_FF 10_000_000";
        let expected_types = vec![
            Int(5),
            Int(10),
            Int(100),
            Float(1.23456),
            Int(15),
            Int(65535),
            Int(10000),
            Int(65535),
            Int(10000000),
        ];

        let mut lexer = Lexer::new(input);
        for expected in expected_types {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected);
        }
    }

    #[test]
    fn sample_program() {
        let input = "
            var five = 5;
            var ten = 10;

            var add = fn(x, y) {
                x + y - 8;
            };

            var result = add(five, ten);

            var foo = 5 * 20 / 100;
            if result > foo {
                return foo != 20;
            } else {
                return foo == 10;
            }
        ";
        let expected_types = vec![
            Variable,
            Identifier("five".to_string()),
            Assign,
            Int(5),
            Semicolon,
            Variable,
            Identifier("ten".to_string()),
            Assign,
            Int(10),
            Semicolon,
            Variable,
            Identifier("add".to_string()),
            Assign,
            Function,
            LParen,
            Identifier("x".to_string()),
            Comma,
            Identifier("y".to_string()),
            RParen,
            LBrace,
            Identifier("x".to_string()),
            Plus,
            Identifier("y".to_string()),
            Minus,
            Int(8),
            Semicolon,
            RBrace,
            Semicolon,
            Variable,
            Identifier("result".to_string()),
            Assign,
            Identifier("add".to_string()),
            LParen,
            Identifier("five".to_string()),
            Comma,
            Identifier("ten".to_string()),
            RParen,
            Semicolon,
            Variable,
            Identifier("foo".to_string()),
            Assign,
            Int(5),
            Asterisk,
            Int(20),
            Slash,
            Int(100),
            Semicolon,
            If,
            Identifier("result".to_string()),
            GreaterThan,
            Identifier("foo".to_string()),
            LBrace,
            Return,
            Identifier("foo".to_string()),
            NotEqual,
            Int(20),
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Return,
            Identifier("foo".to_string()),
            Equal,
            Int(10),
            Semicolon,
            RBrace,
            EOF,
        ];

        let mut lexer = Lexer::new(input);
        for expected in expected_types {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected);
        }
    }
}
