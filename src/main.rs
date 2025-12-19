use crate::repl::Repl;

mod repl;

fn main() {
    let interpreter = Repl::new();
    interpreter.start();
}
