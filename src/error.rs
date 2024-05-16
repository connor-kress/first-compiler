use std::{error::Error, fmt};

#[derive(Debug)]
pub struct CompilationError {
    msg: String,
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Compilation error: {}", self.msg)
    }
}

impl Error for CompilationError {}

impl From<&'static str> for CompilationError {
    fn from(msg: &'static str) -> Self {
        CompilationError { msg: msg.into() }
    }
}

impl From<String> for CompilationError {
    fn from(msg: String) -> Self {
        CompilationError { msg: msg.into() }
    }
}
