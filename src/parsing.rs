pub mod tokenization;
pub use tokenization::{Token, Tokens};

pub use crate::error::CompilationError;

use core::panic;
use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};
use tokenization::PrimativeType;

static UNARY_PREFIX_PRIORITY: u8 = 13;
static UNARY_POSTFIX_PRIORITY: u8 = 14;

lazy_static! {
    static ref MATCHING_CLOSE_TOKEN: HashMap<Token, Token> = HashMap::from([
        (Token::Lparen, Token::Rparen),
        (Token::Lbrack, Token::Rbrack),
        (Token::Lcurly, Token::Rcurly),
    ]);
    static ref BINARY_OPERATOR_PRIORITIES: HashMap<&'static str, u8> = HashMap::from([
        ("=", 1),
        ("||", 2),
        ("&&", 3),
        ("|", 4),
        ("^", 5),
        ("&", 6),
        ("==", 7),
        ("!=", 7),
        ("<=", 8),
        (">=", 8),
        ("<", 8),
        (">", 8),
        ("<<", 9),
        (">>", 9),
        ("+", 10),
        ("-", 10),
        ("*", 11),
        ("/", 11),
        ("%", 11),
        ("**", 12),
        (".", UNARY_POSTFIX_PRIORITY),
    ]);
    static ref UNARY_PREFIX_OPERATORS: HashSet<&'static str> = HashSet::from(["&", "*", "!", "~"]);
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLit(i32),
    FloatLit(f64),
    StrLit(String),
    Symbol(String),
    // AttrAccess(Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    // Assign(Box<Expr>, Box<Expr>),
    BinaryOp(Box<Expr>, Box<Expr>, String),
    UnaryOp(Box<Expr>, String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primative(PrimativeType),
    Symbol(String),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration {
        symbol: String,
        _type: Type,
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

#[derive(Debug, Clone)]
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
    tokens.next().ok_or(msg.into())
}

fn peek_token<'a, It: Iterator<Item = Token>>(
    tokens: &'a mut Peekable<It>,
    msg: &'static str,
) -> Result<&'a Token, CompilationError> {
    tokens.peek().ok_or(msg.into())
}

fn expect_token<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    expected: Token,
    msg: &'static str,
) -> Result<(), CompilationError> {
    if get_token(tokens, msg.into())? != expected {
        Err(msg.into())
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
        Err("Expected indentifier".into())
    }
}
fn parse_type<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
) -> Result<Type, CompilationError> {
    match get_token(tokens, "Expected type expression")? {
        Token::Type(t) => Ok(Type::Primative(t)),
        Token::Symbol(s) => Ok(Type::Symbol(s)),
        _ => Err("Expected type expression".into()),
    }
}

fn parse_numeric_literal(data: &String) -> Result<Expr, CompilationError> {
    if data.is_empty() {
        panic!("Numeric literal is empty");
    }
    if let Ok(i) = data.parse::<i32>() {
        Ok(Expr::IntLit(i))
    } else if let Ok(f) = data.parse::<f64>() {
        Ok(Expr::FloatLit(f))
    } else {
        Err("Error parsing numeric literal".into())
    }
}

fn parse_token_expr(token: &Token) -> Result<Expr, CompilationError> {
    match token {
        Token::Symbol(s) => Ok(Expr::Symbol(s.clone())),
        Token::StrLit(s) => Ok(Expr::StrLit(s.clone())),
        Token::NumLit(s) => Ok(parse_numeric_literal(&s)?),
        _ => {
            let msg = format!("Token ({:?}) does not represent an expression", token);
            Err(msg.into())
        }
    }
}

#[derive(Debug)]
enum BuffElement {
    Expr(Expr),
    ArgList(Vec<Expr>, Token),
    Token(Token),
}

fn valid_unary_prefix_operator(op: &String, prev: Option<&BuffElement>) -> bool {
    if !UNARY_PREFIX_OPERATORS.contains(op.as_str()) {
        println!("Op(`{op}`) is not in unary op set");
        false
    } else if let Some(BuffElement::Token(Token::Op(_))) = prev {
        println!("Op(`{op}`) is unary op b/c prev is an operator");
        true
    } else if prev.is_none() {
        println!("Op(`{op}`) is unary op b/c there is no prev");
        true
    } else {
        println!("Op(`{op}`) is not a unary op b/c no else");
        false
    }
}

fn get_max_priority(buf: &Vec<BuffElement>) -> Result<(u8, bool), CompilationError> {
    let mut max_p = 0;
    let mut right_associative = false;
    let mut prev: Option<&BuffElement> = None;
    for ele in buf {
        if let BuffElement::Token(t) = ele {
            if let Token::Op(s) = t {
                if valid_unary_prefix_operator(s, prev) {
                    if UNARY_PREFIX_PRIORITY > max_p {
                        max_p = UNARY_PREFIX_PRIORITY;
                        right_associative = true;
                    }
                } else if let Some(&op_p) = BINARY_OPERATOR_PRIORITIES.get(s.as_str()) {
                    if op_p > max_p {
                        max_p = op_p;
                        right_associative = false;
                    }
                } else {
                    let msg = format!("Operator `{s}` does not exist");
                    return Err(msg.into());
                }
            } else {
                panic!("Token `{t:?}` should not be in buffer");
            }
        } else if let BuffElement::ArgList(_, _) = ele {
            if let Some(BuffElement::Token(_)) = prev {
                panic!("Unexpected case: ArgList after non-Expr: {prev:?}");
            }
            if prev.is_none() {
                panic!("Unexpected case: ArgList is first element");
            }
            if UNARY_POSTFIX_PRIORITY > max_p {
                max_p = UNARY_POSTFIX_PRIORITY;
                right_associative = false;
            }
        }
        prev = Some(ele);
    }
    if max_p == 0 {
        return Err("No operations found".into());
    }
    Ok((max_p, right_associative))
}

/// Parses atomic expression tokens.
/// ```rust
/// Token::Symbol("foo") => Expr::Symbol("foo")
/// Token::NumLit("3.14") => Expr::FloatLit(3.14)
/// ```
fn parse_atomic_expressions(buf: &mut Vec<BuffElement>) -> Result<(), CompilationError> {
    for ele in buf {
        let BuffElement::Token(t) = ele else {
            continue;
        };
        match *t {
            Token::Op(_) => {}
            Token::Symbol(_) | Token::NumLit(_) | Token::StrLit(_) => {
                *ele = BuffElement::Expr(parse_token_expr(t)?);
            }
            _ => panic!("Did not expect token ({t:?}) while parsing atomic expressions"),
        }
    }
    Ok(())
}

/// Removes single expressions from `ArgList` if they aren't
/// a function/method call.
/// ```rust
/// [Expr::IntLit(1), Token::Op("+"), ArgList([Expr::IntLit(2)], Token::Lparen)]
///
/// [Expr::IntLit(1), Token::Op("+"), Expr::IntLit(2)]
/// ```
fn normalize_arg_lists(buf: &mut Vec<BuffElement>) {
    let mut prev: Option<&mut BuffElement> = None;
    for ele in &mut *buf {
        let BuffElement::ArgList(v, p) = ele else {
            prev = Some(ele);
            continue;
        };
        if *p != Token::Lparen || v.len() != 1 {
            prev = Some(ele);
            continue;
        }
        if prev.is_none() {
            *ele = BuffElement::Expr(v.pop().unwrap());
            prev = Some(ele);
            continue;
        }
        match *prev.unwrap() {
            BuffElement::Expr(_) | BuffElement::ArgList(_, _) => {
                prev = Some(ele);
                continue;
            }
            _ => {}
        }
        *ele = BuffElement::Expr(v.pop().unwrap());
        prev = Some(ele);
    }
}

fn get_binary_operator(
    lhs: Option<&BuffElement>,
    rhs: Option<&BuffElement>,
    op: String,
) -> Result<Expr, CompilationError> {
    if lhs.is_none() {
        let msg =
            format!("Incomplete expression: could not find expression on left side of `{op}`",);
        return Err(msg.into());
    }
    if rhs.is_none() {
        let msg =
            format!("Incomplete expression: could not find expression on right side of `{op}`",);
        return Err(msg.into());
    }
    let BuffElement::Expr(lhs_expr) = lhs.unwrap() else {
        let msg = format!(
            "Expected left side of expression, found {:?} instead",
            lhs.unwrap()
        );
        return Err(msg.into());
    };
    let BuffElement::Expr(rhs_expr) = rhs.unwrap() else {
        let msg = format!(
            "Expected right side of expression, found {:?} instead",
            rhs.unwrap()
        );
        return Err(msg.into());
    };
    Ok(Expr::BinaryOp(
        Box::new(lhs_expr.clone()),
        Box::new(rhs_expr.clone()),
        op,
    ))
}

fn get_unary_prefix_operator(
    next: Option<&BuffElement>,
    op: String,
) -> Result<Expr, CompilationError> {
    let BuffElement::Expr(base_expr) = next.unwrap() else {
        let msg = format!(
            "Expected unary modifier on expression, found {:?} instead",
            next.unwrap()
        );
        return Err(msg.into());
    };
    Ok(Expr::UnaryOp(Box::new(base_expr.clone()), op))
}

// fn get_attr_access(
//     prev: Option<&BuffElement>,
//     next: Option<&BuffElement>,
// ) -> Result<Expr, CompilationError> {
//     if prev.is_none() || next.is_none() {
//         return Err("Unexpected dot".into());
//     }
//     let BuffElement::Expr(base_expr) = prev.unwrap() else {
//         let msg = format!(
//             "Expected attribute access on expression, found {:?} instead",
//             prev.unwrap()
//         );
//         return Err(msg.into());
//     };
//     let BuffElement::Expr(Expr::Symbol(attr_name)) = next.unwrap() else {
//         let msg = format!("Expected attribute name, found {:?} instead", next.unwrap());
//         return Err(msg.into());
//     };
//     Ok(Expr::AttrAccess(
//         Box::new(base_expr.clone()),
//         attr_name.clone(),
//     ))
// }

fn get_index_expression(
    prev: Option<&BuffElement>,
    args: &Vec<Expr>,
) -> Result<Expr, CompilationError> {
    if args.len() != 1 {
        return Err("Array literals not implemented".into());
    }
    let Some(BuffElement::Expr(base_expr)) = prev else {
        return Err("Expected array expression prior to array index".into());
    };
    Ok(Expr::Index(
        Box::new(base_expr.clone()),
        Box::new(args.first().unwrap().clone()),
    ))
}

fn get_call_expression(
    prev: Option<&BuffElement>,
    args: &Vec<Expr>,
) -> Result<Expr, CompilationError> {
    let Some(BuffElement::Expr(base_expr)) = prev else {
        let msg =
            format!("Expected expression prior to method or function call (found `{prev:?}`)");
        return Err(msg.into());
    };
    Ok(Expr::Call(Box::new(base_expr.clone()), args.clone()))
}

fn recursively_get_buffer<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    closing: Option<Token>,
) -> Result<Vec<BuffElement>, CompilationError> {
    let mut buf = Vec::new();
    use Token as T;
    loop {
        let token = peek_token(tokens, "Unfinished expression")?;
        buf.push(match *token {
            T::Symbol(_) | T::Op(_) | T::NumLit(_) | T::StrLit(_) => {
                BuffElement::Token(tokens.next().unwrap())
            }
            T::Lparen | T::Lbrack => {
                let opening_token = tokens.next().unwrap();
                BuffElement::ArgList(
                    parse_arg_list(tokens, opening_token.clone())?,
                    opening_token,
                )
            }
            T::Rparen | T::Rbrack => {
                if closing.is_none() || closing.unwrap() != *token {
                    return Err("Unexpected token".into());
                }
                break;
            }
            T::Comma => {
                if closing.is_none() {
                    return Err("Unexpected comma".into());
                }
                break;
            }
            T::Semi => {
                if closing.is_some() {
                    return Err("Unexpected Semicolon".into());
                }
                break;
            }
            T::While | T::For | T::If | T::Else | T::Return | T::Lcurly | T::Rcurly => {
                return Err("Unexpected token".into());
            }
            T::Type(_) => todo!(),
        });
    }
    Ok(buf)
}

fn parse_expr<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    closing: Option<Token>,
) -> Result<Expr, CompilationError> {
    let mut buf = recursively_get_buffer(tokens, closing)?;
    parse_atomic_expressions(&mut buf)?;
    normalize_arg_lists(&mut buf);

    println!("{:?}", &buf);
    while buf.len() > 1 {
        let (max_p, right_associative) = get_max_priority(&buf)?;
        dbg!(max_p, right_associative);
        let mut i = if right_associative { buf.len() - 1 } else { 0 };
        let step = |i: &mut usize| {
            if right_associative {
                *i -= 1;
                if *i == 0 {
                    false
                } else {
                    true
                }
            } else {
                *i += 1;
                true
            }
        };
        while i < buf.len() {
            println!("{:?}", &buf);
            dbg!(i);
            let curr = buf.get(i).unwrap();
            let prev = if i > 0 { buf.get(i - 1) } else { None };
            let next = buf.get(i + 1);
            if let BuffElement::Token(t) = curr {
                if let Token::Op(s) = t {
                    println!("Op(\"{s}\") found");
                    if valid_unary_prefix_operator(s, prev) && max_p == UNARY_PREFIX_PRIORITY {
                        println!("Op(\"{s}\") is a unary prefix");
                        let expr = get_unary_prefix_operator(next, s.clone())?;
                        buf.remove(i);
                        buf.remove(i);
                        buf.insert(i, BuffElement::Expr(expr));
                    } else if BINARY_OPERATOR_PRIORITIES.get(s.as_str()).is_none() {
                        panic!("Cannot identify operator `{s}`");
                    } else if *BINARY_OPERATOR_PRIORITIES.get(s.as_str()).unwrap() == max_p {
                        let expr = get_binary_operator(prev, next, s.clone())?;
                        buf.remove(i - 1);
                        buf.remove(i - 1);
                        buf.remove(i - 1);
                        buf.insert(i - 1, BuffElement::Expr(expr));
                        i -= 1;
                    } else {
                        if !step(&mut i) {
                            break;
                        }
                    }
                } else {
                    panic!("Unexpeccted token ({t:?})");
                }
            } else if let BuffElement::ArgList(v, p) = curr {
                if let Token::Lparen = p {
                    if max_p != UNARY_POSTFIX_PRIORITY {
                        if !step(&mut i) {
                            break;
                        }
                        continue;
                    }
                    let expr = get_call_expression(prev, v)?;
                    buf.remove(i - 1);
                    buf.remove(i - 1);
                    buf.insert(i - 1, BuffElement::Expr(expr));
                    i -= 1;
                } else if let Token::Lbrack = p {
                    if max_p != UNARY_POSTFIX_PRIORITY {
                        if !step(&mut i) {
                            break;
                        }
                        continue;
                    }
                    let expr = get_index_expression(prev, v)?;
                    buf.remove(i - 1);
                    buf.remove(i - 1);
                    buf.insert(i - 1, BuffElement::Expr(expr));
                    i -= 1;
                } else {
                    panic!("Unexpeccted bracket type ({p:?})");
                }
            } else {
                // BuffElement::Expr
                if !step(&mut i) {
                    break;
                }
            }
        }
    }
    use BuffElement as BE;
    match buf
        .pop()
        .ok_or(CompilationError::from("Empty expression"))?
    {
        BE::Token(t) => Ok(parse_token_expr(&t)?),
        BE::Expr(e) => Ok(e),
        BE::ArgList(mut v, p) => {
            if v.len() != 1 {
                Err("Expected single expression".into())
            } else if p != Token::Lparen {
                Err("Expected expression".into())
            } else {
                Ok(v.pop().unwrap())
            }
        }
    }
}

fn parse_arg_list<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
    opening_token: Token,
) -> Result<Vec<Expr>, CompilationError> {
    let closing_token = MATCHING_CLOSE_TOKEN.get(&opening_token).unwrap();
    use Token as T;
    if let T::Rparen | T::Rbrack = peek_token(tokens, "Expected first token of expression")? {
        let _ = tokens.next().unwrap();
        return Ok(Vec::new());
    }
    let mut args = Vec::new();
    loop {
        args.push(parse_expr(tokens, Some(closing_token.clone()))?);
        match get_token(tokens, "Failed to close argument list")? {
            T::Comma => {}
            T::Rparen | T::Rbrack => break,
            _ => return Err("Unexpected token in arg list".into()),
        }
    }
    Ok(args)
}

fn parse_statement<It: Iterator<Item = Token>>(
    tokens: &mut Peekable<It>,
) -> Result<Statement, CompilationError> {
    let expr = parse_expr(tokens, None)?;
    dbg!(expr);
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
            Token::Comma => {}
            Token::Rparen => break,
            _ => return Err("Expected parameter".into()),
        }
    }
    let body = parse_block(tokens)?;
    Ok(Function {
        name,
        return_t,
        params,
        body,
    })
}
