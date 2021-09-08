use super::ast::*;
use super::function::*;
use super::codegen::*;
use super::keyword::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    Print,
    PrintI32Test,
    Println,
}

pub fn gen_builtin_il(kind: Builtin, args: Vec<Node>, f: &[Function]) -> Type {
    match kind {
        Builtin::Print   => gen_print_il(args, f),
        Builtin::PrintI32Test => gen_printi32_test_il(args, f),
        Builtin::Println => gen_println_il(args, f),
    }
}

fn gen_printi32_test_il(args: Vec<Node>, f: &[Function]) -> Type {
    if args.len() != 1 {
        panic!("printi32_test: support only one arg");
    }
    for arg in args {
        gen_il(arg, f);
    }
    println!("\tcall void [mscorlib]System.Console::Write(int32)");
    Type::Void
}

fn gen_print_il(args: Vec<Node>, f: &[Function]) -> Type {
    let argc = args.len();
    for arg in args {
        gen_il(arg, f);
    }
    print!("\tcall void [mscorlib]System.Console::Write(");
    for i in 0..argc {
        print!("string{}", if i+1<argc{","}else{""});
    }
    println!(")");
    Type::Void
}

fn gen_println_il(args: Vec<Node>, f: &[Function]) -> Type {
    let argc = args.len();
    for arg in args {
        gen_il(arg, f);
    }
    print!("\tcall void [mscorlib]System.Console::WriteLine(");
    for i in 0..argc {
        print!("string{}", if i+1<argc{","}else{""});
    }
    println!(")");
    Type::Void
}
