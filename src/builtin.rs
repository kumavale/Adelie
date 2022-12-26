use crate::ast::*;
use crate::codegen::*;
use crate::error::*;
use crate::keyword::{Type, Numeric};
use crate::program::Program;
use crate::token::{LiteralKind, Token, TokenKind};
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

pub fn gen_il_builtin<'a>(token: &[Token], kind: Builtin, args: Vec<Node>, p: &'a Program<'a>) -> Result<Type> {
    match kind {
        Builtin::Assert   => gen_il_builtin_assert(token, args, p),
        Builtin::AssertEq => gen_il_builtin_assert_eq(token, args, p),
        Builtin::Panic    => gen_il_builtin_panic(token, args, p),
        Builtin::Print    => gen_il_builtin_print(token, args, p),
        Builtin::Println  => gen_il_builtin_println(token, args, p),
        Builtin::ReadLine => gen_il_builtin_read_line(token, args, p),
    }
}

/// assertion failed: {arg}
fn gen_il_builtin_assert<'a>(token: &[Token], mut args: Vec<Node>, p: &'a Program<'a>) -> Result<Type> {
    if args.len() != 1 {
        e0029(Rc::clone(&p.errors), (p.path, &p.lines, token), 1, args.len());
        return Err(());
    }
    let arg = args.pop().unwrap();
    let stringizing_arg = arg.token.iter().map(|t|format!("{}",t.kind)).collect::<Vec<_>>().join(" ");
    let ty = gen_il(arg, p)?;
    if ty != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ty);
    }
    p.push_il_text(format!("\tldstr \"{stringizing_arg}\""));
    p.push_il_text(format!("\tldstr \"{}:{}:{}\"", p.path, token[0].line, token[0].cur));
    p.push_il_text("\tcall void [adelie_std]std::'assert'(bool, string, string)");
    Ok(Type::Void)
}

fn gen_il_builtin_assert_eq<'a>(token: &[Token], mut args: Vec<Node>, p: &'a Program<'a>) -> Result<Type> {
    fn check_type(lty: &Type, rty: &Type) -> std::result::Result<(), ()> {
        match (&lty, &rty) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if lty == rty => Ok(()),
            _ => Err(())
        }
    }
    if args.len() != 2 {
        e0029(Rc::clone(&p.errors), (p.path, &p.lines, token), 2, args.len());
        return Err(());
    }
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();
    let lty = gen_il(lhs, p)?;
    p.push_il_text(format!("\tbox {}", lty.to_ilstr()));
    let rty = gen_il(rhs, p)?;
    p.push_il_text(format!("\tbox {}", rty.to_ilstr()));
    if check_type(&lty, &rty).is_err() {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &lty, &rty);
    }
    p.push_il_text(format!("\tldstr \"{}:{}:{}\"", p.path, token[0].line, token[0].cur));
    p.push_il_text("\tcall void [adelie_std]std::'assert_eq'(object, object, string)");
    Ok(Type::Void)
}

/// panicked at '{msg}', src/main.rs:2:5
fn gen_il_builtin_panic<'a>(token: &[Token], mut args: Vec<Node>, p: &'a Program<'a>) -> Result<Type> {
    let argc = args.len();
    match argc {
        0 => p.push_il_text("\tldstr \"explicit panic\""),
        1 => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            // check arg counts
            if format_arg_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            p.push_il_text("\tldstr \"{{0}}\"");
            let ty = gen_il(format_shaping(format), p)?;
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
            if format_arg_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            gen_il(format_shaping(format), p)?;
            p.push_il_text(format!("\tldc.i4 {}", argc));
            p.push_il_text("\tnewarr object");
            for (i, arg) in args.into_iter().enumerate() {
                p.push_il_text("\tdup");
                p.push_il_text(format!("\tldc.i4 {}", i));
                let ty = gen_il(arg, p)?;
                p.push_il_text(format!("\tbox {}", ty.to_ilstr()));
                p.push_il_text("\tstelem.ref");
            }
            p.push_il_text("\tcall string [mscorlib]System.String::Format(string, object[])");
        }
    }
    p.push_il_text(format!("\tldstr \"{}:{}:{}\"", p.path, token[0].line, token[0].cur));
    p.push_il_text("\tcall void [adelie_std]std::'panic'(string, string)");

    // TODO: Type::Never
    Ok(Type::Void)
}

fn gen_il_builtin_print<'a>(_token: &[Token], args: Vec<Node>, p: &'a Program<'a>) -> Result<Type> {
    format_args(_token, args, p, false)
}

fn gen_il_builtin_println<'a>(_token: &[Token], args: Vec<Node>, p: &'a Program<'a>) -> Result<Type> {
    format_args(_token, args, p, true)
}

fn gen_il_builtin_read_line<'a>(token: &[Token], args: Vec<Node>, p: &'a Program) -> Result<Type> {
    if !args.is_empty() {
        e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "read_line! takes no arguments");
    }
    p.push_il_text("\tcall string [mscorlib]System.Console::ReadLine()");
    Ok(Type::String)
}

fn format_args<'a>(_token: &[Token], mut args: Vec<Node>, p: &'a Program<'a>, nl: bool) -> Result<Type> {
    let nl = if nl { "Line" } else { "" };
    let argc = args.len();
    match argc {
        0 => p.push_il_text(format!("\tcall void [mscorlib]System.Console::Write{nl}()")),
        1 => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            // check arg counts
            if format_arg_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            let ty = gen_il(format_shaping(format), p)?;
            p.push_il_text(format!("\tcall void [mscorlib]System.Console::Write{nl}({})",
                match ty {
                    Type::Numeric(n) => n.to_ilstr(),
                    Type::Float(f) => f.to_ilstr(),
                    Type::Char | Type::Bool | Type::String => ty.to_ilstr(),
                    b @ Type::Box(_) => b.to_ilstr(),
                    _ => {
                        dbg!(ty);
                        unimplemented!();
                    }
                }));
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            if !matches!(token[0].kind, TokenKind::Literal(LiteralKind::String(_))) {
                // format argument must be a string literal
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "format argument must be a string literal");
            }
            // check arg counts
            if format_arg_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            gen_il(format_shaping(format), p)?;
            p.push_il_text(format!("\tldc.i4 {}", argc));
            p.push_il_text("\tnewarr object");
            for (i, arg) in args.into_iter().enumerate() {
                p.push_il_text("\tdup");
                p.push_il_text(format!("\tldc.i4 {}", i));
                let ty = gen_il(arg, p)?;
                p.push_il_text(format!("\tbox {}", ty.to_ilstr()));
                p.push_il_text("\tstelem.ref");
            }
            p.push_il_text(format!("\tcall void [mscorlib]System.Console::Write{nl}(string, object[])"));
        }
    }
    Ok(Type::Void)
}

/// Return `{}` count
fn format_arg_count(node: &Node) -> usize {
    match &node.kind {
        NodeKind::String{ ty: _, str } => str.matches("{}").count(),
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
