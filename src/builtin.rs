use std::fmt;
use super::ast::*;
use super::codegen::*;
use super::keyword::*;
use super::program::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    Print,
    Println,
    ReadLine,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Print    => write!(f, "print"),
            Builtin::Println  => write!(f, "println"),
            Builtin::ReadLine => write!(f, "read_line"),
        }
    }
}

pub fn gen_builtin_il(kind: Builtin, args: Vec<Node>, p: &Program) -> Type {
    match kind {
        Builtin::Print    => gen_print_il(args, p),
        Builtin::Println  => gen_println_il(args, p),
        Builtin::ReadLine => gen_read_line_il(args, p),
    }
}

fn gen_print_il(mut args: Vec<Node>, p: &Program) -> Type {
    let argc = args.len();
    match argc {
        0 => println!("\tcall void [mscorlib]System.Console::Write()"),
        1 => {
            gen_il(args.drain(..1).next().unwrap(), p);
            println!("\tcall void [mscorlib]System.Console::Write(string)");
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            gen_il(format, p);
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
            gen_il(args.drain(..1).next().unwrap(), p);
            println!("\tcall void [mscorlib]System.Console::WriteLine(string)");
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            gen_il(format, p);
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
    if args.is_empty() {
        panic!();
    }
    println!("\tcall string [mscorlib]System.Console::ReadLine()");
    Type::String
}
