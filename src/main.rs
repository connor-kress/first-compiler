use core::fmt;
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::process::exit;

#[derive(Debug)]
struct CompilationError {
    msg: String,
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Compilation error: {}", self.msg)
    }
}

impl Error for CompilationError {}

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
    dbg!(&data);
    exit(exitcode::OK);
}
