use std::fmt;
use super::ast::*;
use super::codegen::*;
use super::error::*;
use super::keyword::*;
use super::program::*;
use super::token::*;

#[derive(Clone, Copy, Debug, PartialEq)]
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

pub fn gen_builtin_il(token: &[Token], kind: Builtin, args: Vec<Node>, p: &Program) -> Type {
    match kind {
        Builtin::Assert   => gen_assert_il(token, args, p),
        Builtin::AssertEq => gen_assert_eq_il(token, args, p),
        Builtin::Print    => gen_print_il(token, args, p),
        Builtin::Println  => gen_println_il(token, args, p),
        Builtin::ReadLine => gen_read_line_il(token, args, p),
    }
}

fn gen_assert_il(token: &[Token], mut args: Vec<Node>, p: &Program) -> Type {
    if args.len() != 1 {
        todo!();
    }
    gen_il(args.pop().unwrap(), p);
    println!("\tcall void [System.Diagnostics.Debug]System.Diagnostics.Debug::Assert(bool)");
    Type::Void
}

fn gen_assert_eq_il(token: &[Token], mut args: Vec<Node>, p: &Program) -> Type {
    if args.len() != 2 {
        todo!();
    }
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();
    gen_il(new_binary_op_node(BinaryOpKind::Eq, lhs, rhs, &[]), p);
    println!("\tcall void [System.Diagnostics.Debug]System.Diagnostics.Debug::Assert(bool)");
    Type::Void
}

fn gen_print_il(token: &[Token], mut args: Vec<Node>, p: &Program) -> Type {
    let argc = args.len();
    match argc {
        0 => println!("\tcall void [mscorlib]System.Console::Write()"),
        1 => {
            let ty = gen_il(args.drain(..1).next().unwrap(), p);
            println!("\tcall void [mscorlib]System.Console::Write({})",
                match ty {
                    Type::Numeric(..) => Numeric::I32.to_ilstr(),
                    Type::Char | Type::Bool | Type::String => ty.to_ilstr(),
                    _ => unimplemented!()
                });
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            if !matches!(token[0].kind, TokenKind::Literal(LiteralKind::String(_))) {
                // format argument must be a string literal
                e0000((p.path, &p.lines, token), "format argument must be a string literal");
            }
            let fmtty = gen_il(format, p);
            if fmtty != Type::String {
                e0012((p.path, &p.lines, token), &Type::String, &fmtty);
            }
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
            println!("\tcall void [mscorlib]System.Console::Write(string, object[])");
        }
    }
    Type::Void
}

fn gen_println_il(token: &[Token], mut args: Vec<Node>, p: &Program) -> Type {
    let argc = args.len();
    match argc {
        0 => println!("\tcall void [mscorlib]System.Console::WriteLine()"),
        1 => {
            let ty = gen_il(args.drain(..1).next().unwrap(), p);
            println!("\tcall void [mscorlib]System.Console::WriteLine({})",
                match ty {
                    Type::Numeric(..) => Numeric::I32.to_ilstr(),
                    Type::Char | Type::Bool | Type::String => ty.to_ilstr(),
                    _ => unimplemented!()
                });
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            let token = format.token;
            if !matches!(token[0].kind, TokenKind::Literal(LiteralKind::String(_))) {
                // format argument must be a string literal
                e0000((p.path, &p.lines, token), "format argument must be a string literal");
            }
            let fmtty = gen_il(format, p);
            if fmtty != Type::String {
                e0012((p.path, &p.lines, token), &Type::String, &fmtty);
            }
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
            println!("\tcall void [mscorlib]System.Console::WriteLine(string, object[])");
        }
    }
    Type::Void
}

fn gen_read_line_il(token: &[Token], args: Vec<Node>, p: &Program) -> Type {
    if !args.is_empty() {
        e0000((p.path, &p.lines, token), "read_line! takes no arguments");
    }
    println!("\tcall string [mscorlib]System.Console::ReadLine()");
    Type::String
}
