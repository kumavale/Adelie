mod ast;
mod builtin;
mod class;
mod codegen;
mod error;
mod function;
mod keyword;
mod lexer;
mod namespace;
mod object;
mod parser;
mod program;
mod token;
mod utils;

use crate::error::Errors;
use crate::keyword::{Type, Numeric};
use crate::namespace::NameSpace;
use crate::program::Program;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let input = std::fs::read_to_string(&path).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    //eprintln!("{:?}", tokens.iter().map(|t|t.kind.clone()).collect::<Vec<token::TokenKind>>());
    //eprintln!("{:?}", tokens);

    let errors = Rc::new(RefCell::new(Errors::new()));
    let mut g_symbol_table = object::SymbolTable::new();
    let mut parser = parser::Parser::new(&path, &input, &tokens, &mut g_symbol_table, errors);
    let program = parser.gen_ast();

    if !program.errors.borrow().is_empty() {
        disp_errors(&program);
    }
    //eprintln!("{:#?}", program);

    gen_manifest(&program, Path::new(&path));
    gen_builtin();
    gen_items(&program, &program.namespace.borrow());

    if !program.errors.borrow().is_empty() {
        disp_errors(&program);
    }
}

fn gen_manifest<'a>(program: &'a Program<'a>, path: &Path) {
    // 組み込み関数で使用
    println!(".assembly extern mscorlib {{}}");
    println!(".assembly extern System.Diagnostics.Debug {{");
    println!("    .publickeytoken = (B0 3F 5F 7F 11 D5 0A 3A)");
    println!("}}");

    for assembly in &program.references {
        let name = assembly.find_value("name").unwrap();
        println!(".assembly extern {} {{", &name[..name.len()-4]);
        if let Some(pkt) = assembly.find_value("publickeytoken") {
            println!("    .publickeytoken = ({})", pkt);
        }
        println!("}}");
    }

    let assembly_name = path
        .file_stem()
        .and_then(|n|n.to_str())
        .unwrap_or_default();
    println!(".assembly '{}' {{}}", assembly_name);
}

fn gen_builtin() {
    // panic!()
    println!(".method public static hidebysig specialname void '<adelie>panic'(string msg, string locate) cil managed {{");
    println!("    .maxstack 4");
    println!("    ldstr \"paniced at '\"");
    println!("    ldarg msg");
    println!("    ldstr \"', \"");
    println!("    ldarg locate");
    println!("    call string [mscorlib]System.String::Concat(string, string, string, string)");
    println!("    call void [mscorlib]System.Console::WriteLine(string)");
    println!("    ldc.i4 101");
    println!("    call void [mscorlib]System.Environment::Exit(int32)");
    println!("    ret");
    println!("}}");
}

fn gen_items<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    if namespace.is_foreign {
        return;
    }
    gen_structs(program, namespace);
    gen_impls(program, namespace);
    gen_functions(program, namespace);
    for child in &namespace.children {
        gen_items(program, &child.borrow());
    }
}

fn gen_structs<'a, 'b>(_program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for st in &namespace.structs {
        println!(".class private sequential auto sealed beforefieldinit {} extends System.ValueType", st.name);
        println!("{{");
        for value in &st.field {
            println!("\t.field public {} {}", value.ty.borrow().to_ilstr(), value.name);
        }
        println!("}}");
    }
}

fn gen_impls<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for im in &namespace.impls {
        println!(".class private sequential auto sealed beforefieldinit {} extends System.ValueType", im.name);
        println!("{{");
        for func in &im.functions {
            let args = func
                .param_symbol_table
                .objs
                .iter()
                .skip(if func.is_static { 0 } else { 1 })
                .map(|o|format!("{} {}", o.borrow().ty.borrow().to_ilstr(), o.borrow().name))
                .collect::<Vec<String>>()
                .join(", ");
            println!("\t.method public {} {} {}({}) cil managed {{",
                if func.is_static {"static"} else {"instance"},
                func.rettype.borrow().to_ilstr(),
                func.name,
                args);
            println!("\t\t.maxstack 32");

            let rettype = codegen::gen_il(func.statements.clone(), program);
            match (&rettype, &*func.rettype.borrow()) {
                (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                _ => if rettype != *func.rettype.borrow() {
                    panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.borrow(), rettype);
                }
            }
            println!("\t\tret");

            // prepare local variables
            let locals = func
                .lvar_symbol_table
                .borrow()
                .objs
                .iter()
                .enumerate()
                .map(|(i, obj)| format!("\t\t\t{} V_{}", obj.borrow().ty.borrow().to_ilstr(), i))
                .collect::<Vec<String>>()
                .join(",\n");
            if !locals.is_empty() {
                println!("\t\t.locals init (");
                println!("{}", locals);
                println!("\t\t)");
            }

            println!("\t}}");
        }
        println!("}}");
    }
}

fn gen_functions<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for func in &namespace.functions {
        if func.name == "main" {
            println!(".method static void Main() cil managed {{");
            println!("\t.entrypoint");
        } else {
            let args = func
                .param_symbol_table
                .objs
                .iter()
                .map(|o|format!("{} {}", o.borrow().ty.borrow().to_ilstr(), o.borrow().name))
                .collect::<Vec<String>>()
                .join(", ");
            println!(".method static {} {}({}) cil managed {{", func.rettype.borrow().to_ilstr(), func.name, args);
        }
        println!("\t.maxstack 32");

        let rettype = codegen::gen_il(func.statements.clone(), program);
        match (&rettype, &*func.rettype.borrow()) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
            _ => if rettype != *func.rettype.borrow() {
                panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.borrow(), rettype);
            }
        }
        println!("\tret");

        // prepare local variables
        let locals = func
            .lvar_symbol_table
            .borrow()
            .objs
            .iter()
            .enumerate()
            .map(|(i, obj)| format!("\t\t{} V_{}", obj.borrow().ty.borrow().to_ilstr(), i))
            .collect::<Vec<String>>()
            .join(",\n");
        if !locals.is_empty() {
            println!("\t.locals init (");
            println!("{}", locals);
            println!("\t)");
        }

        println!("}}");
    }
}

fn disp_errors(program: &Program) {
    let err_count = program.errors.borrow().err_count();
    program.errors.borrow().display();
    eprintln!("\x1b[31merror\x1b[0m: could not compile due to {} previous errors", err_count);
    std::process::exit(1);
}
