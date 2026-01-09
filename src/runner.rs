use std::fs;

use crate::compiler::compile;
use crate::lexer::lex_input;
use crate::parser::parse_program;
use crate::vm::VM;

pub fn run_script(filename: &str) -> Result<(), Vec<String>> {
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file: {}", filename));

    let tokens = lex_input(&contents);
    let ast = parse_program(tokens)?;
    let bytecode = compile(&ast)?;

    let mut vm = VM::new(bytecode);
    let result = vm.run();
    match result {
        Err(s) => Err(vec![s]),
        _ => Ok(()),
    }
}
