use std::cell::RefCell;
use std::io::{Write, stdin, stdout};
use std::rc::Rc;

use crate::lexer::lex_input;
use crate::{environment::Environment, evaluator::evaluate, parser::Parser};

pub struct Repl {}

impl Repl {
    pub fn new() -> Repl {
        Repl {}
    }

    pub fn start(&self) {
        let mut buffer = String::new();
        let environment = Rc::new(RefCell::new(Environment::new()));

        loop {
            print!(">> ");
            let _ = stdout().flush().unwrap();

            buffer.clear();
            stdin().read_line(&mut buffer).unwrap();
            let tokens = lex_input(&buffer);
            let mut parser = Parser::new(tokens);

            let program = parser.parse_program();
            if parser.has_errors() {
                parser.print_errors();
                continue;
            }

            let obj = evaluate(&program, Rc::clone(&environment));
            println!("{}", obj.inspect());
        }
    }
}
