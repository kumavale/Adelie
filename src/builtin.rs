use super::ast::*;
use super::function::*;
use super::codegen::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    Print,
    PrintI32Test,
    Println,
}

pub fn gen_builtin_il(kind: Builtin, args: Vec<Node>, fst: &FunctionSymbolTable) {
    match kind {
        Builtin::Print   => gen_print_il(args, fst),
        Builtin::PrintI32Test => gen_printi32_test_il(args, fst),
        Builtin::Println => gen_println_il(args, fst),
    }
}

fn gen_printi32_test_il(args: Vec<Node>, fst: &FunctionSymbolTable) {
    if args.len() != 1 {
        panic!("printi32_test: support only one arg");
    }
    for arg in args {
        gen_il(arg, fst);
    }
    println!("\tcall void [mscorlib]System.Console::Write(int32)");
    println!("\tldc.i4.0");
}

fn gen_print_il(args: Vec<Node>, fst: &FunctionSymbolTable) {
    let argc = args.len();
    for arg in args {
        gen_il(arg, fst);
    }
    print!("\tcall void [mscorlib]System.Console::Write(");
    for i in 0..argc {
        print!("string{}", if i+1<argc{","}else{""});
    }
    println!(")");
    println!("\tldc.i4.0");
}

fn gen_println_il(args: Vec<Node>, fst: &FunctionSymbolTable) {
    let argc = args.len();
    for arg in args {
        gen_il(arg, fst);
    }
    print!("\tcall void [mscorlib]System.Console::WriteLine(");
    for i in 0..argc {
        print!("string{}", if i+1<argc{","}else{""});
    }
    println!(")");
    println!("\tldc.i4.0");
}
