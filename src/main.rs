use std::{env, fs, process::exit};

use scanner::Scanner;

mod scanner;
mod parser;
mod ast;

fn main() {
    let mut args: Vec<String> = env::args().collect();
    let str = args.pop().unwrap();
    println!("COCK");
    run(str)
}

fn run_file(path: &str) {
    let script = fs::read_to_string(path);
}

fn run(args: String) {
    print!("{}", args);

    let mut scanner = Scanner::new(args);
    let tokens = scanner.scan_tokens();

    println!("{:?}", tokens);
}
