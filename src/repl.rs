use std::io::{Write, stdin, stdout};

use crate::{ast::Statement, lexer::Lexer, parser::Parser};

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
            let lexer = Lexer::new(&buffer);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            if parser.has_errors() {
                parser.print_errors();
            }

            let Statement::Program(statements) = program else {
                panic!("parse_program needs to return a Statement::Program");
            };

            for statement in statements {
                println!("{:?}", statement);
            }
        }
    }
}
