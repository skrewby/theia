use std::cell::RefCell;
use std::io::{Write, stdin, stdout};
use std::rc::Rc;

use crate::lexer::lex_input;
use crate::parser::parse_program;
use crate::{environment::Environment, evaluator::evaluate};

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
            let program = match parse_program(tokens) {
                Ok(p) => p,
                Err(errors) => {
                    for e in errors {
                        println!("{}", e);
                    }
                    continue;
                }
            };

            let obj = evaluate(&program, Rc::clone(&environment));
            println!("{}", obj.inspect());
        }
    }
}
