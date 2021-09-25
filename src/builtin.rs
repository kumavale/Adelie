use std::fmt;
use super::ast::*;
use super::codegen::*;
use super::error::*;
use super::keyword::*;
use super::program::*;

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

pub fn gen_builtin_il(kind: Builtin, args: Vec<Node>, p: &Program) -> Type {
    match kind {
        Builtin::Assert   => gen_assert_il(args, p),
        Builtin::AssertEq => gen_assert_eq_il(args, p),
        Builtin::Print    => gen_print_il(args, p),
        Builtin::Println  => gen_println_il(args, p),
        Builtin::ReadLine => gen_read_line_il(args, p),
    }
}

fn gen_assert_il(mut args: Vec<Node>, p: &Program) -> Type {
    if args.len() != 1 {
        todo!();
    }
    gen_il(args.pop().unwrap(), p);
    println!("\ncall void [System.Diagnostics.Debug]System.Diagnostics.Debug::Assert(bool)");
    Type::Void
}

fn gen_assert_eq_il(mut args: Vec<Node>, p: &Program) -> Type {
    if args.len() != 2 {
        todo!();
    }
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();
    gen_il(new_binary_op_node(BinaryOpKind::Eq, lhs, rhs, &[]), p);
    println!("\ncall void [System.Diagnostics.Debug]System.Diagnostics.Debug::Assert(bool)");
    Type::Void
}

fn gen_print_il(mut args: Vec<Node>, p: &Program) -> Type {
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
            let fmtty = gen_il(format, p);
            if fmtty != Type::String {
                e0012(("[TODO: path]", &p.lines, token), &Type::String, &fmtty);
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

fn gen_println_il(mut args: Vec<Node>, p: &Program) -> Type {
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
            let fmtty = gen_il(format, p);
            if fmtty != Type::String {
                e0012(("[TODO: path]", &p.lines, token), &Type::String, &fmtty);
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

fn gen_read_line_il(args: Vec<Node>, _p: &Program) -> Type {
    if !args.is_empty() {
        panic!();
    }
    println!("\tcall string [mscorlib]System.Console::ReadLine()");
    Type::String
}
