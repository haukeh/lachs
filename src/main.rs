use std::{env, fs, io, process::exit, error::Error};

use ast::Interpreter;
use scanner::Scanner;

use crate::parser::Parser;

mod ast;
mod parser;
mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: lachs [script]");
        exit(64)
    } else if args.len() == 2 {
        if let Err(e) = run_file(&args[1]) {
            println!("[ERROR] {e}");
            exit(70)
        }
    } else {
        run_prompt();
    }
    exit(0)
}

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(path)?;
    run(source)
}

fn run_prompt() {
    println!("> welcome to lachs");
    loop {        
        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("Failed to read line");
        if let Err(e) = run(line) {
            println!("[ERROR] {e}");
        }
    }
}

fn run(line: String) -> Result<(), Box<dyn Error>> {
    let scanner = Scanner::new(line);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse()?;
    let interpreter = Interpreter::new();
    interpreter.interpret(&stmts);
    Ok(())
}
