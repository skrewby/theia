use std::io::{Write, stdin, stdout};

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
            println!("{}", buffer);
        }
    }
}
