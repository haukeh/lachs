use std::{env, fs, io, process::exit, error::Error};

use ast::{Interpreter, Stmt};
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
    let stmts = parse(source)?;
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&stmts);    
    Ok(())
}

fn run_prompt() {
    println!("> welcome to lachs");
    let mut interpreter = Interpreter::new();    
    loop {        
        let mut stmt = String::new();
        io::stdin()
            .read_line(&mut stmt)
            .expect("Failed to read line");
        match parse(stmt) {
            Ok(stmt) => interpreter.interpret(&stmt),
            Err(e) => println!("[ERROR] {e}"),
        }       
    }
}

fn parse(source: String) -> Result<Vec<Stmt>, Box<dyn Error>> {
    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    Ok(parser.parse()?)
}
