use crate::ast::*;
use crate::codegen::*;
use crate::error::*;
use crate::keyword::{Type, RRType, Numeric, Float};
use crate::object::SymbolTable;
use crate::program::Program;
use crate::token::{LiteralKind, Token, TokenKind};
use crate::typing::{typing, type_inference};
use std::fmt;
use std::rc::Rc;

type Result<T> = std::result::Result<T, ()>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Builtin {
    Assert,
    AssertEq,
    Panic,
    Print,
    Println,
    ReadLine,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Assert   => write!(f, "assert"),
            Builtin::AssertEq => write!(f, "assert_eq"),
            Builtin::Panic    => write!(f, "panic"),
            Builtin::Print    => write!(f, "print"),
            Builtin::Println  => write!(f, "println"),
            Builtin::ReadLine => write!(f, "read_line"),
        }
    }
}

pub fn gen_il_builtin<'a>(token: &[Token], st: &SymbolTable, kind: Builtin, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    match kind {
        Builtin::Assert   => gen_il_builtin_assert(token, st, args, p),
        Builtin::AssertEq => gen_il_builtin_assert_eq(token, st, args, p),
        Builtin::Panic    => gen_il_builtin_panic(token, st, args, p),
        Builtin::Print    => gen_il_builtin_print(token, st, args, p),
        Builtin::Println  => gen_il_builtin_println(token, st, args, p),
        Builtin::ReadLine => gen_il_builtin_read_line(token, st, args, p),
    }
}

/// assertion failed: {arg}
fn gen_il_builtin_assert<'a>(token: &[Token], st: &SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    if args.len() != 1 {
        e0029(Rc::clone(&p.errors), (p.path, &p.lines, token), 1, args.len());
        return Err(());
    }
    let arg = args.pop().unwrap();
    let stringizing_arg = arg.token.iter().map(|t|format!("{}",t.kind)).collect::<Vec<_>>().join(" ");
    let ty = gen_il(arg, st, p, false)?;
    if ty.get_type() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ty.get_type());
    }
    p.push_il_text(format!("\tldstr \"{stringizing_arg}\""));
    p.push_il_text(format!("\tldstr \"{}:{}:{}\"", p.path, token[0].line, token[0].cur.start));
    p.push_il_text("\tcall void [adelie_std]std::'assert'(bool, string, string)");
    Ok(RRType::new(Type::Void))
}

fn gen_il_builtin_assert_eq<'a>(token: &[Token], st: &SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    fn check_type(lty: &Type, rty: &Type) -> std::result::Result<(), ()> {
        match (&lty, &rty) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(()),
            (Type::Float(Float::F), Type::Float(..)) => Ok(()),
            (Type::Float(..), Type::Float(Float::F)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.get_type(), &r.get_type()),
            _ if lty == rty => Ok(()),
            _ => Err(())
        }
    }
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();
    let lty = gen_il(lhs, st, p, false)?;
    p.push_il_text(format!("\tbox {}", lty.to_ilstr()));
    let rty = gen_il(rhs, st, p, false)?;
    p.push_il_text(format!("\tbox {}", rty.to_ilstr()));
    if check_type(&lty.get_type(), &rty.get_type()).is_err() {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &lty.get_type(), &rty.get_type());
    }
    p.push_il_text(format!("\tldstr \"{}:{}:{}\"", p.path, token[0].line, token[0].cur.start));
    p.push_il_text("\tcall void [adelie_std]std::'assert_eq'(object, object, string)");
    Ok(RRType::new(Type::Void))
}

/// panicked at '{msg}', src/main.rs:2:5
fn gen_il_builtin_panic<'a>(token: &[Token], st: &SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => p.push_il_text("\tldstr \"explicit panic\""),
        1 => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            // check arg counts
            if format_placeholder_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            p.push_il_text("\tldstr \"{{0}}\"");
            let ty = gen_il(format_shaping(format), st, p, false)?;
            p.push_il_text(format!("\tbox {}", ty.to_ilstr()));
            p.push_il_text("\tcall string [mscorlib]System.String::Format(string, object)");
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            if !matches!(token[0].kind, TokenKind::Literal(LiteralKind::String(_))) {
                // format argument must be a string literal
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "format argument must be a string literal");
            }
            // check arg counts
            if format_placeholder_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            gen_il(format_shaping(format), st, p, false)?;
            p.push_il_text(format!("\tldc.i4 {}", argc));
            p.push_il_text("\tnewarr object");
            for (i, arg) in args.into_iter().enumerate() {
                p.push_il_text("\tdup");
                p.push_il_text(format!("\tldc.i4 {}", i));
                let ty = gen_il(arg, st, p, false)?;
                p.push_il_text(format!("\tbox {}", ty.to_ilstr()));
                p.push_il_text("\tstelem.ref");
            }
            p.push_il_text("\tcall string [mscorlib]System.String::Format(string, object[])");
        }
    }
    p.push_il_text(format!("\tldstr \"{}:{}:{}\"", p.path, token[0].line, token[0].cur.start));
    p.push_il_text("\tcall void [adelie_std]std::'panic'(string, string)");

    // TODO: Type::Never
    Ok(RRType::new(Type::Void))
}

fn gen_il_builtin_print<'a>(_token: &[Token], st: &SymbolTable, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    format_args(_token, st, args, p)
}

fn gen_il_builtin_println<'a>(_token: &[Token], st: &SymbolTable, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    format_args_nl(_token, st, args, p)
}

fn gen_il_builtin_read_line<'a>(_token: &[Token], _st: &SymbolTable, _args: Vec<Node>, p: &'a Program) -> Result<RRType> {
    p.push_il_text("\tcall string [mscorlib]System.Console::ReadLine()");
    Ok(RRType::new(Type::String))
}

fn format_args<'a>(token: &[Token], st: &SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => {
            let message = "requires at least a format string argument";
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), message);
        }
        1 => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            // check place holder counts
            if format_placeholder_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            if let NodeKind::String{..} = &format.kind {
                if let Some(FmtKind::Literal(s)) = &fmt_tokenize(token, p, format).first() {
                    p.push_il_text(format!("\tldstr \"{}\"", s));
                    p.push_il_text("\tcall void [mscorlib]System.Console::Write(string)");
                } else {
                    // maybe compile error
                    debug_assert!(p.errors.borrow().any_deny());
                }
            } else {
                let ty = gen_il(format, st, p, false)?.get_type();
                match &ty {
                    Type::Numeric(_) |
                    Type::Float(_)   |
                    Type::Char       |
                    Type::Bool       |
                    Type::String     => {
                        p.push_il_text(format!("\tcall void [mscorlib]System.Console::WriteLine({})", ty.to_ilstr()));
                    }
                    _ => {
                        let message = format!("[compiler unimplemented!()] cannot print: {:?}", ty);
                        e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), &message);
                    }
                }
            }
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            if !matches!(token[0].kind, TokenKind::Literal(LiteralKind::String(_))) {
                // format argument must be a string literal
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "format argument must be a string literal");
            }
            // check place holder counts
            if format_placeholder_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            let fmt_tokens = fmt_tokenize(token, p, format);
            let mut args = args.into_iter();
            for tok in fmt_tokens {
                match tok {
                    FmtKind::Literal(s) => {
                        p.push_il_text(format!("\tldstr \"{}\"", s));
                        p.push_il_text("\tcall void [mscorlib]System.Console::Write(string)");
                    }
                    FmtKind::PlaceHolder => {
                        let ty = gen_il(args.next().unwrap(), st, p, false)?;
                        p.push_il_text(format!("\tcall void [mscorlib]System.Console::Write({})", ty.to_ilstr()));
                    }
                }
            }
        }
    }
    Ok(RRType::new(Type::Void))
}

fn format_args_nl<'a>(_token: &[Token], st: &SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => p.push_il_text("\tcall void [mscorlib]System.Console::WriteLine()"),
        1 => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            // check place holder counts
            if format_placeholder_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            if let NodeKind::String{..} = &format.kind {
                if let Some(FmtKind::Literal(s)) = &fmt_tokenize(token, p, format).first() {
                    p.push_il_text(format!("\tldstr \"{}\"", s));
                    p.push_il_text("\tcall void [mscorlib]System.Console::WriteLine(string)");
                } else {
                    // maybe compile error
                    debug_assert!(p.errors.borrow().any_deny());
                }
            } else {
                let ty = gen_il(format, st, p, true)?.get_type();
                match &ty {
                    Type::Numeric(_) |
                    Type::Float(_)   |
                    Type::Char       |
                    Type::Bool       |
                    Type::String     => {
                        p.push_il_text(format!("\tcall instance string {}::ToString()", ty.to_ilstr()));
                        p.push_il_text("\tcall void [mscorlib]System.Console::WriteLine(string)");
                    }
                    //Type::String => {
                    //    p.push_il_text(format!("\tcall void [mscorlib]System.Console::WriteLine(string)"));
                    //}
                    _ => {
                        let message = format!("[compiler unimplemented!()] cannot print: {:?}", ty);
                        e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), &message);
                    }
                }
            }
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            let fmt_tokens = fmt_tokenize(token, p, format);
            let tokens_len = fmt_tokens.len();
            let mut args = args.into_iter();
            for (i, tok) in fmt_tokens.iter().enumerate() {
                let nl = if i == tokens_len-1 { "Line" } else { "" };
                match tok {
                    FmtKind::Literal(s) => {
                        p.push_il_text(format!("\tldstr \"{}\"", s));
                        p.push_il_text(format!("\tcall void [mscorlib]System.Console::Write{nl}(string)"));
                    }
                    FmtKind::PlaceHolder => {
                        let ty = gen_il(args.next().unwrap(), st, p, true)?.get_type();
                        match &ty {
                            Type::Numeric(_) |
                            Type::Float(_)   |
                            Type::Char       |
                            Type::Bool       |
                            Type::String     => {
                                p.push_il_text(format!("\tcall instance string {}::ToString()", ty.to_ilstr()));
                                p.push_il_text(format!("\tcall void [mscorlib]System.Console::Write{nl}(string)"));
                            }
                            //Type::String => {
                            //    p.push_il_text(format!("\tcall void [mscorlib]System.Console::WriteLine(string)"));
                            //}
                            _ => {
                                let message = format!("[compiler unimplemented!()] cannot print: {:?}", ty);
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), &message);
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(RRType::new(Type::Void))
}

pub fn typing_builtin<'a>(token: &[Token], st: &mut SymbolTable, kind: Builtin, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    match kind {
        Builtin::Assert   => typing_builtin_assert(token, st, args, p),
        Builtin::AssertEq => typing_builtin_assert_eq(token, st, args, p),
        Builtin::Panic    => typing_builtin_panic(token, st, args, p),
        Builtin::Print    => typing_builtin_print(token, st, args, p),
        Builtin::Println  => typing_builtin_println(token, st, args, p),
        Builtin::ReadLine => typing_builtin_read_line(token, st, args, p),
    }
}

fn typing_builtin_assert<'a>(token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    if args.len() != 1 {
        e0029(Rc::clone(&p.errors), (p.path, &p.lines, token), 1, args.len());
        return Err(());
    }
    let arg = args.pop().unwrap();
    let ty = typing(arg, st, p, false)?;
    type_inference(&mut RRType::new(Type::Bool), &ty);
    Ok(RRType::new(Type::Void))
}

fn typing_builtin_assert_eq<'a>(token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    if args.len() != 2 {
        e0029(Rc::clone(&p.errors), (p.path, &p.lines, token), 2, args.len());
        return Err(());
    }
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();
    let mut lty = typing(lhs, st, p, false)?;
    let mut rty = typing(rhs, st, p, false)?;
    type_inference(&mut lty, &rty);
    type_inference(&mut rty, &lty);
    Ok(RRType::new(Type::Void))
}

fn typing_builtin_panic<'a>(_token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => (),
        1 => {
            let format = args.drain(..1).next().unwrap();
            typing(format, st, p, false)?;
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            typing(format, st, p, false)?;
            for arg in args {
                typing(arg, st, p, false)?;
            }
        }
    }
    Ok(RRType::new(Type::Void))
}

fn typing_builtin_print<'a>(_token: &[Token], st: &mut SymbolTable, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    typing_format_args(_token, st, args, p)
}

fn typing_builtin_println<'a>(_token: &[Token], st: &mut SymbolTable, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    typing_format_args_nl(_token, st, args, p)
}

fn typing_builtin_read_line<'a>(token: &[Token], _st: &mut SymbolTable, args: Vec<Node>, p: &'a Program) -> Result<RRType> {
    if !args.is_empty() {
        e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "read_line! takes no arguments");
    }
    Ok(RRType::new(Type::String))
}

fn typing_format_args<'a>(_token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => (),
        1 => {
            let format = args.drain(..1).next().unwrap();
            typing(format, st, p, false)?;
        }
        _ => {
            for arg in args {
                typing(arg, st, p, false)?;
            }
        }
    }
    Ok(RRType::new(Type::Void))
}

fn typing_format_args_nl<'a>(_token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => (),
        1 => {
            let format = args.drain(..1).next().unwrap();
            typing(format, st, p, true)?;
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            if !matches!(token[0].kind, TokenKind::Literal(LiteralKind::String(_))) {
                // format argument must be a string literal
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "format argument must be a string literal");
            }
            // check place holder counts
            if format_placeholder_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            let fmt_tokens = fmt_tokenize(token, p, format);
            let mut args = args.into_iter();
            for tok in fmt_tokens {
                match tok {
                    FmtKind::Literal(_) => (),
                    FmtKind::PlaceHolder => {
                        let _ty = typing(args.next().unwrap(), st, p, true)?.get_type();
                    }
                }
            }
        }
    }
    Ok(RRType::new(Type::Void))
}

/// Return `{}` count
// TODO: `{.*}`
fn format_placeholder_count(node: &Node) -> usize {
    match &node.kind {
        NodeKind::String{ ty: _, str } => str.replace("{{", "").matches("{}").count(),
        _ => 0,
    }
}

/// `{}` to `{n}`
fn format_shaping(mut node: Node) -> Node {
    match &node.kind {
        NodeKind::String{ ty, str } => {
            let mut arg_count = 0;
            let mut s = str.to_string();
            while let Some(idx) = s.find("{}") {
                s.remove(idx);
                s.remove(idx);
                s.insert_str(idx, &format!("{{{arg_count}}}"));
                arg_count += 1;
            }
            node.kind = NodeKind::String { ty: ty.clone(), str: s };
            node
        }
        _ => node
    }
}

#[derive(Debug)]
enum FmtKind {
    Literal(String),
    PlaceHolder,
}

/// e.g.
///
/// ```
/// let t = fmt_tokennize("1+1={}")
/// assert_eq!(t, &[FmtKind::Literal("1+1="), FmtKind::PlaceHolder]);
/// ```
///
/// ```
/// let t = fmt_tokennize("{{}}")
/// assert_eq!(t, &[FmtKind::Literal("{}")]);
/// ```
///
/// ```
/// let t = fmt_tokennize("{{{}}}")
/// assert_eq!(t, &[FmtKind::Literal("{"), FmtKind::PlaceHolder, FmtKind::Literal("}")]);
/// ```
fn fmt_tokenize<'a>(token: &[Token], p: &'a Program<'a>, node: Node) -> Vec<FmtKind> {
    match &node.kind {
        NodeKind::String { ty: _, str } => {
            let mut tokens = vec![];
            let mut chars = str.chars().peekable();
            while let Some(c) = chars.next() {
                match c {
                    '{' => match chars.next() {
                        Some('}') => {
                            tokens.push(FmtKind::PlaceHolder);
                        }
                        Some('{') => {
                            if let Some(FmtKind::Literal(s)) = tokens.last_mut() {
                                s.push('{');
                            } else {
                                tokens.push(FmtKind::Literal("{".to_string()));
                            }
                        }
                        _ => {
                            // TODO: Formatting Parameters
                            let message = "unimplemented formatting parameters";
                            e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), message);
                            // skip until close
                            while chars.next_if(|&c|c!='}').is_some() {}
                            chars.next();
                        }
                    }
                    '}' => match chars.next() {
                        Some('}') => {
                            if let Some(FmtKind::Literal(s)) = tokens.last_mut() {
                                s.push('}');
                            } else {
                                tokens.push(FmtKind::Literal("}".to_string()));
                            }
                        }
                        _ => {
                            let message = "invalid format string: unmatched `}` found";
                            e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), message);
                        }
                    }
                    _ => {
                        let mut lit = c.to_string();
                        while let Some(c) = chars.next_if(|&c|c!='{'&&c!='}') {
                            lit.push(c);
                        }
                        if let Some(FmtKind::Literal(s)) = tokens.last_mut() {
                            s.push_str(&lit);
                        } else {
                            tokens.push(FmtKind::Literal(lit));
                        }
                    }
                }
            }
            tokens
        }
        _ => vec![]
    }
}
