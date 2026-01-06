use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

use crate::environment::Environment;
use crate::evaluator::evaluate;
use crate::lexer::lex_input;
use crate::object::Object;
use crate::parser::parse_program;

pub fn run_script(filename: &str) -> Result<(), Vec<String>> {
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file: {}", filename));

    let tokens = lex_input(&contents);
    let program = parse_program(tokens)?;

    let env = Rc::new(RefCell::new(Environment::new()));
    let result = evaluate(&program, env);

    match result {
        Object::Error(s) => Err(vec![s]),
        _ => Ok(()),
    }
}
