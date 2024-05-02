pub mod tokenization;
pub use tokenization::{Token, Tokens};

use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt,
    iter::Peekable,
};
use tokenization::PrimativeType;

lazy_static! {
    static ref ALLOW_COMMA_SEPERATED: HashSet<Token> = HashSet::from([Token::Rparen]);
    static ref MATCHING_CLOSE_TOKEN: HashMap<Token, Token> = HashMap::from([
        (Token::Lparen, Token::Rparen),
        (Token::Lbrack, Token::Rbrack),
        (Token::Lcurly, Token::Rcurly),
    ]);
}

#[derive(Debug)]
pub struct CompilationError {
    msg: &'static str,
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Compilation error: {}", self.msg)
    }
}

impl Error for CompilationError {}

impl CompilationError {
    pub fn new(msg: &'static str) -> Self {
        CompilationError { msg }
    }
}

#[derive(Debug)]
pub enum Expr {
    IntLit(i32),
    FloatLit(f64),
    Symbol(String),
}

#[derive(Debug)]
pub enum Type {
    Primative(PrimativeType),
    Symbol(String),
}

#[derive(Debug)]
pub enum Statement {
    Declaration {
        symbol: String,
        _type: Type,
    },
    Assignment {
        assigned: Expr,
        value: Expr,
    },
    Conditional {
        condition: Expr,
        _if: Vec<Statement>,
        _else: Vec<Statement>,
    },
    WhileLoop {
        condition: Expr,
        body: Vec<Statement>,
    },
    ForLoop {
        init: Box<Statement>,
        condition: Expr,
        step: Box<Statement>,
        body: Vec<Statement>,
    },
    Return {
        value: Expr,
    },
    VoidExpr {
        value: Expr,
    },
}

#[derive(Debug)]
pub struct Function {
    name: String,
    return_t: Type,
    params: Vec<(Type, String)>,
    body: Vec<Statement>,
}

// pub struct Module {
//     definitions: Vec<Function>,
// }

fn get_token<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    msg: &'static str,
) -> Result<Token, CompilationError> {
    tokens.next().ok_or(CompilationError::new(msg.into()))
}

fn peek_token<'a, It: Iterator<Item = Token>>(
    tokens: &'a mut Peekable<It>,
    msg: &'static str,
) -> Result<&'a Token, CompilationError> {
    tokens.peek().ok_or(CompilationError::new(msg.into()))
}

fn expect_token<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    expected: Token,
    msg: &'static str,
) -> Result<(), CompilationError> {
    if get_token(tokens, msg.into())? != expected {
        Err(CompilationError::new(msg.into()))
    } else {
        Ok(())
    }
}

fn parse_symbol<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
) -> Result<String, CompilationError> {
    if let Token::Symbol(name) = get_token(tokens, "Expected identifier")? {
        Ok(name)
    } else {
        Err(CompilationError::new("Expected indentifier"))
    }
}
fn parse_type<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
) -> Result<Type, CompilationError> {
    match get_token(tokens, "Expected type expression")? {
        Token::Type(t) => Ok(Type::Primative(t)),
        Token::Symbol(s) => Ok(Type::Symbol(s)),
        _ => Err(CompilationError::new("Expected type expression")),
    }
}

#[derive(Debug)]
enum BuffElement {
    Expr(Expr),
    ArgList(Vec<Expr>, Token),
    Token(Token),
}

/// put tokens into buffer of enum {Token, Expr}
/// until end token (semi or (comma or lparen))
/// and parse expressions in parentheses
/// maybe statements in curly brackets (parse_block)
/// recursively into Expr enum variant
fn parse_expr<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    closing: Option<Token>,
) -> Result<Expr, CompilationError> {
    let mut buff = Vec::new();
    use BuffElement as BE;
    use Token as T;
    loop {
        let token = peek_token(tokens, "Unfinished expression")?;
        buff.push(match *token {
            T::Symbol(_) | T::Op(_) | T::NumLit(_) | T::StrLit(_) | T::Dot => {
                BE::Token(tokens.next().unwrap())
            }
            T::Lparen | T::Lbrack => {
                let opening_token = tokens.next().unwrap();
                BE::ArgList(
                    parse_arg_list(tokens, opening_token.clone())?,
                    opening_token,
                )
            }
            T::Rparen | T::Rbrack => {
                if closing.is_none() || closing.unwrap() != *token {
                    return Err(CompilationError::new("Unexpected token"));
                }
                println!("breaking because ) or ]");
                break;
            }
            T::Comma => {
                if closing.is_none() || !ALLOW_COMMA_SEPERATED.contains(&closing.unwrap()) {
                    return Err(CompilationError::new("Unexpected comma"));
                }
                println!("breaking because comma");
                break;
            }
            T::Semi => {
                if closing.is_some() {
                    return Err(CompilationError::new("Unexpected Semicolon"));
                }
                println!("breaking because semicolon");
                break;
            }
            T::While | T::For | T::If | T::Return | T::Lcurly | T::Rcurly => {
                return Err(CompilationError::new("Unexpected token"));
            }
            T::Type(_) => todo!(),
        });
    }
    println!("{:?}", buff);
    todo!()
}

fn parse_arg_list<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    opening_token: Token,
) -> Result<Vec<Expr>, CompilationError> {
    let closing_token = MATCHING_CLOSE_TOKEN.get(&opening_token).unwrap();
    let mut args = Vec::new();
    use Token as T;
    loop {
        args.push(parse_expr(tokens, Some(closing_token.clone()))?);
        match get_token(tokens, "Failed to close argument list")? {
            T::Comma => Ok(()),
            T::Rparen | T::Rbrack => break,
            _ => Err(CompilationError::new("Unexpected token")),
        }?
    }
    Ok(args)
}

fn parse_statement<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
) -> Result<Statement, CompilationError> {
    let _ = parse_expr(tokens, None)?;
    todo!()
}

fn parse_block<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
) -> Result<Vec<Statement>, CompilationError> {
    expect_token(tokens, Token::Lcurly, "Expected function body")?;
    let mut block = Vec::new();
    while *peek_token(tokens, "Failed to close function body")? != Token::Rcurly {
        block.push(parse_statement(tokens)?);
    }
    return Ok(block);
}

pub fn parse_function<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
) -> Result<Function, CompilationError> {
    tokens.peek().expect("First token not found");
    let return_t = parse_type(tokens)?;
    let name = parse_symbol(tokens)?;
    expect_token(tokens, Token::Lparen, "Expected parameter list")?;
    let mut params = Vec::new();
    loop {
        let _type = parse_type(tokens)?;
        let symbol = parse_symbol(tokens)?;
        params.push((_type, symbol));
        match get_token(tokens, "Failed to close parameter list")? {
            Token::Comma => Ok(()),
            Token::Rparen => break,
            _ => Err(CompilationError::new("Expected parameter")),
        }?
    }
    let body = parse_block(tokens)?;
    Ok(Function {
        name,
        return_t,
        params,
        body,
    })
}
