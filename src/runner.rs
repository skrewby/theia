use std::cell::RefCell;
use std::fs;
use std::rc::Rc;

use crate::environment::Environment;
use crate::evaluator::evaluate;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;

pub fn run_script(filename: &str) {
    let contents =
        fs::read_to_string(filename).expect(&format!("Failed to read file: {}", filename));

    let lexer = Lexer::new(&contents);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if parser.has_errors() {
        parser.print_errors();
        std::process::exit(1);
    }

    let env = Rc::new(RefCell::new(Environment::new()));
    let result = evaluate(&program, env);

    match result {
        Object::Null => {}
        _ => println!("{}", result.inspect()),
    }
}
