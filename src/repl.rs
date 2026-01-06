use std::io::{Write, stdin, stdout};

use crate::compiler::compile;
use crate::lexer::lex_input;
use crate::parser::parse_program;
use crate::vm::VM;

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

            let bytecode = match compile(&program) {
                Ok(p) => p,
                Err(errors) => {
                    for e in errors {
                        println!("{}", e);
                    }
                    continue;
                }
            };

            let mut vm = VM::new(bytecode);
            let obj = match vm.run() {
                Ok(o) => o,
                Err(e) => {
                    println!("{}", e);
                    continue;
                }
            };

            println!("{}", obj.inspect());
        }
    }
}
