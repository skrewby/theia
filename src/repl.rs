use std::io::{Write, stdin, stdout};
use std::mem;

use crate::compiler::compile_with_state;
use crate::compiler::symbol_table::SymbolTable;
use crate::lexer::lex_input;
use crate::object::Object;
use crate::parser::parse_program;
use crate::vm::VM;

pub struct Repl {}

impl Repl {
    pub fn new() -> Repl {
        Repl {}
    }

    pub fn start(&self) {
        let mut buffer = String::new();
        let mut constants: Vec<Object> = Vec::new();
        let mut symbol_table = SymbolTable::new();
        let mut globals: Vec<Object> = Vec::new();
        symbol_table.register_builtins();

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

            let current_symbol_table = mem::replace(&mut symbol_table, SymbolTable::new());
            let current_constants = mem::replace(&mut constants, Vec::new());

            let result = match compile_with_state(&program, current_symbol_table, current_constants)
            {
                Ok(r) => r,
                Err(errors) => {
                    for e in errors {
                        println!("{}", e);
                    }
                    continue;
                }
            };

            symbol_table = result.symbol_table;
            constants = result.bytecode.constants.clone();

            let current_globals = mem::replace(&mut globals, Vec::new());
            let mut vm = VM::new_with_state(result.bytecode, current_globals);
            let obj = match vm.run() {
                Ok(o) => o,
                Err(e) => {
                    println!("{}", e);
                    continue;
                }
            };

            globals = vm.take_globals();

            println!("{}", obj.inspect());
        }
    }
}
