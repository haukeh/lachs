use std::{env, error::Error, fs, io::{self, Write}, process::exit};

use ast::Stmt;
use interpreter::Interpreter;
use resolver::Resolver;
use scanner::Scanner;

use crate::parser::Parser;

mod ast;
mod interpreter;
mod parser;
mod resolver;
mod scanner;

fn main() {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    match args.len() {
        l if l > 2 => {
            println!("Usage: lachs [script]");
            exit(64)
        }
        2 => {
            if let Err(e) = run_file(&args[1]) {
                println!("[ERROR] {e}");
                exit(70)
            }
        }
        _ => {
            run_prompt();
        }
    }
    exit(0)
}

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let mut stdout = io::stdout().lock();
    let source = fs::read_to_string(path)?;    
    run(source, &mut stdout)
}

fn run_prompt() {
    println!("> welcome to lachs");
    let mut stdout = io::stdout().lock();
    let mut interpreter = Interpreter::new(&mut stdout);
    loop {
        let mut stmt = String::new();
        io::stdin()
            .read_line(&mut stmt)
            .expect("Failed to read line");

        if let Err(e) =
            parse(stmt).and_then(|stmt| interpreter.interpret(&stmt).map_err(|err| err.into()))
        {
            println!("[ERROR] {e}")
        }
    }
}

fn run(source: String, out: &mut dyn Write) -> Result<(), Box<dyn Error>> {
    let stmts = parse(source)?;
    let mut interpreter = Interpreter::new(out);
    let mut resolver = Resolver::new(&mut interpreter);
    resolver.resolve_stmts(&stmts);
    
    interpreter.interpret(&stmts)?;
    
    Ok(())
}

fn parse(source: String) -> Result<Vec<Stmt>, Box<dyn Error>> {
    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(tokens);
    Ok(parser.parse()?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn closures_must_not_leak() {
        let prog = r###"
var a = "global";
{
    fun showA() {
        print a;
    }

    showA();
    var a = "block";
    showA();
}
"###;
        let mut out = Vec::new();
        run(prog.to_string(), &mut out).unwrap();        
        
        assert_eq!("global\nglobal\n", String::from_utf8_lossy(&out));
    }
}
