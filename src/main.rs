use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::process::exit;

mod parsing;
use parsing::{Token, Tokens};

mod error;

use crate::parsing::parse_function;

fn get_content(path_str: &String) -> std::io::Result<String> {
    let base_path = env::current_dir()?;
    let user_path = PathBuf::from(path_str);
    let path = base_path.join(user_path);
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!(
            "Invalid syntax. Correct usage is...\n
            \tfcomp <path to file>"
        );
        exit(exitcode::USAGE);
    }
    let path_str = args.get(1).unwrap();
    let data = get_content(path_str).unwrap_or_else(|_| {
        eprintln!("File not found: {}", path_str);
        exit(exitcode::DATAERR);
    });
    // dbg!(&data);
    let tokens: Vec<Token> = Tokens::from(data.chars()).collect();
    println!("{:?}", &tokens);
    // for token in tokens {
    //     println!("{:?}", token);
    // }
    let func = parse_function(&mut tokens.clone().into_iter().peekable());
    println!("{:?}", func);

    exit(exitcode::OK);
}
