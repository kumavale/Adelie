use crate::ast::*;
use crate::codegen::*;
use crate::error::*;
use crate::keyword::Type;
use crate::program::Program;
use crate::token::{LiteralKind, Token, TokenKind};
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Builtin {
    Assert,
    AssertEq,
    Print,
    Println,
    ReadLine,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Assert   => write!(f, "assert"),
            Builtin::AssertEq => write!(f, "assert_eq"),
            Builtin::Print    => write!(f, "print"),
            Builtin::Println  => write!(f, "println"),
            Builtin::ReadLine => write!(f, "read_line"),
        }
    }
}

pub fn gen_il_builtin<'a>(token: &[Token], kind: Builtin, args: Vec<Node>, p: &'a Program<'a>) -> Type {
    match kind {
        Builtin::Assert   => gen_il_builtin_assert(token, args, p),
        Builtin::AssertEq => gen_il_builtin_assert_eq(token, args, p),
        Builtin::Print    => gen_il_builtin_print(token, args, p),
        Builtin::Println  => gen_il_builtin_println(token, args, p),
        Builtin::ReadLine => gen_il_builtin_read_line(token, args, p),
    }
}

fn gen_il_builtin_assert<'a>(_token: &[Token], mut args: Vec<Node>, p: &'a Program<'a>) -> Type {
    if args.len() != 1 {
        todo!();
    }
    gen_il(args.pop().unwrap(), p);
    println!("\tcall void [System.Diagnostics.Debug]System.Diagnostics.Debug::Assert(bool)");
    Type::Void
}

fn gen_il_builtin_assert_eq<'a>(_token: &[Token], mut args: Vec<Node>, p: &'a Program<'a>) -> Type {
    if args.len() != 2 {
        todo!();
    }
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();
    gen_il(new_binary_op_node(BinaryOpKind::Eq, lhs, rhs, &[]), p);
    println!("\tcall void [System.Diagnostics.Debug]System.Diagnostics.Debug::Assert(bool)");
    Type::Void
}

fn gen_il_builtin_print<'a>(_token: &[Token], args: Vec<Node>, p: &'a Program<'a>) -> Type {
    format_args(_token, args, p, false)
}

fn gen_il_builtin_println<'a>(_token: &[Token], args: Vec<Node>, p: &'a Program<'a>) -> Type {
    format_args(_token, args, p, true)
}

fn gen_il_builtin_read_line<'a>(token: &[Token], args: Vec<Node>, p: &'a Program) -> Type {
    if !args.is_empty() {
        e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "read_line! takes no arguments");
    }
    println!("\tcall string [mscorlib]System.Console::ReadLine()");
    Type::String
}

fn format_args<'a>(_token: &[Token], mut args: Vec<Node>, p: &'a Program<'a>, nl: bool) -> Type {
    let nl = if nl { "Line" } else { "" };
    let argc = args.len();
    match argc {
        0 => println!("\tcall void [mscorlib]System.Console::Write{nl}()"),
        1 => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            // check arg counts
            if format_arg_count(&format) != argc-1 {
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "invalid format");
            }
            let ty = gen_il(format_shaping(format), p);
            println!("\tcall void [mscorlib]System.Console::Write{nl}({})",
                match ty {
                    Type::Numeric(n) => n.to_ilstr(),
                    Type::Char | Type::Bool | Type::String => ty.to_ilstr(),
                    b @ Type::Box(_) => b.to_ilstr(),
                    _ => unimplemented!()
                });
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
            gen_il(format_shaping(format), p);
            println!("\tldc.i4 {}", argc);
            println!("\tnewarr object");
            args.into_iter()
                .enumerate()
                .for_each(|(i, arg)| {
                    println!("\tdup");
                    println!("\tldc.i4 {}", i);
                    let ty = gen_il(arg, p);
                    println!("\tbox {}", ty.to_ilstr());
                    println!("\tstelem.ref");
                });
            println!("\tcall void [mscorlib]System.Console::Write{nl}(string, object[])");
        }
    }
    Type::Void
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
