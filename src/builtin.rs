use super::ast::*;
use super::function::*;
use super::codegen::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    Println,
}

pub fn gen_builtin_il(kind: Builtin, args: Vec<Box<Node>>, fst: &FunctionSymbolTable) {
    match kind {
        Builtin::Println => gen_println_il(args, fst),
    }
}

fn gen_println_il(args: Vec<Box<Node>>, fst: &FunctionSymbolTable) {
    let argc = args.len();
    for arg in args {
        gen_il(*arg, fst);
    }
    print!("\tcall void [mscorlib]System.Console::WriteLine(");
    for i in 0..argc {
        print!("string{}", if i+1<argc{","}else{""});
    }
    println!(")");
    println!("\tldc.i4.0");
}
