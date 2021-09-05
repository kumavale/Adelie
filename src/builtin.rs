use super::ast::*;
use super::codegen::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Builtin {
    Println,
}

pub fn gen_builtin_il(kind: Builtin, args: Vec<Box<Node>>) {
    match kind {
        Builtin::Println => gen_println_il(args),
    }
}

fn gen_println_il(args: Vec<Box<Node>>) {
    let argc = args.len();
    for arg in args {
        gen_il(*arg);
    }
    print!("\tcall void [mscorlib]System.Console::WriteLine(");
    for i in 0..argc {
        print!("string{}", if i+1<argc{","}else{""});
    }
    println!(")");
    println!("ldc.i4.0");
}
