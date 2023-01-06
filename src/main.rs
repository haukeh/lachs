use std::{env, fs, io, process::exit, error::Error};

use scanner::Scanner;

use crate::{ast::AstPrinter, parser::Parser};

mod ast;
mod parser;
mod scanner;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("Usage: lachs [script]");
        exit(64)
    } else if args.len() == 2 {
        run_file(args.first().unwrap())
    } else {
        run_prompt();
    }
}

fn run_file(path: &str) {
    let _ = fs::read_to_string(path);
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
    let expr = parser.parse()?;
    AstPrinter::new().print(&expr);
    Ok(())
}
