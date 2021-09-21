use std::fmt;
use super::ast::*;
use super::codegen::*;
use super::keyword::*;
use super::program::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    AssertEq,
    Print,
    Println,
    ReadLine,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::AssertEq => write!(f, "assert_eq"),
            Builtin::Print    => write!(f, "print"),
            Builtin::Println  => write!(f, "println"),
            Builtin::ReadLine => write!(f, "read_line"),
        }
    }
}

pub fn gen_builtin_il(kind: Builtin, args: Vec<Node>, p: &Program) -> Type {
    match kind {
        Builtin::AssertEq => gen_assert_eq_il(args, p),
        Builtin::Print    => gen_print_il(args, p),
        Builtin::Println  => gen_println_il(args, p),
        Builtin::ReadLine => gen_read_line_il(args, p),
    }
}

fn gen_assert_eq_il(mut args: Vec<Node>, p: &Program) -> Type {
    if args.len() != 2 {
        panic!("missing arguments");
    }
    //println!("\t.locals init (int32 __left, int32 __right)");
    let rtype = gen_il(args.pop().unwrap(), p);
    // TODO: save right hand side value
    //println!("\tldloc __left");
    let ltype = gen_il(args.pop().unwrap(), p);
    // TODO: save left hand side value
    //println!("\tldloc __right");
    if ltype != rtype {
        panic!("expected `{}`, found `{}`", ltype, rtype);
    }
    println!("\tcall void assert_eq<{}>(!!0, !!0)", ltype.to_ilstr());
    //match ltype {
    //    Type::Bool | Type::Char | Type::Numeric(_) => println!("\tceq"),
    //    _ => todo!("cmp string")
    //}
    ////println!("\ncall void [System.Diagnostics.Debug]System.Diagnostics.Debug::Assert(bool)");
    //let end_label = format!("IL_end{}", seq());
    //println!("\tbrtrue {}", end_label);
    //// TODO: error message
    ////gen_println_il
    //// TODO: panic
    //println!("{}:", end_label);
    Type::Void
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
