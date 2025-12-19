use std::io::{Write, stdin, stdout};

use crate::{lexer::Lexer, token::TokenType};

pub struct Repl {}

impl Repl {
    pub fn new() -> Repl {
        Repl {}
    }

    pub fn start(&self) {
        let mut buffer = String::new();
        loop {
            print!(">> ");
            let _ = stdout().flush().unwrap();

            buffer.clear();
            stdin().read_line(&mut buffer).unwrap();
            let mut lexer = Lexer::new(&buffer);
            loop {
                let token = lexer.next_token();
                if matches!(token.token_type, TokenType::EOF) {
                    break;
                }
                println!("{:?}", token);
            }
        }
    }
}
