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

use crate::class::ClassKind;
use crate::error::Errors;
use crate::function::Function;
use crate::keyword::{Type, Numeric};
use crate::namespace::NameSpace;
use crate::program::Program;
use std::cell::RefCell;
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

    gen_manifest(&program);
    gen_builtin();
    gen_items(&program, &program.namespace.borrow());

    if !program.errors.borrow().is_empty() {
        disp_errors(&program);
    }
}

fn gen_manifest<'a>(program: &'a Program<'a>) {
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

    println!(".assembly '{}' {{}}", program.name);
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
    gen_functions(program, namespace);
    for child in &namespace.children {
        gen_items(program, &child.borrow());
    }
}

fn gen_structs<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for st in &namespace.classes {
        if st.borrow().kind != ClassKind::Struct {
            continue;
        }
        println!(".class private sequential auto sealed beforefieldinit '{}' extends [System.Runtime]System.ValueType", st.borrow().name);
        println!("{{");
        for value in &st.borrow().field.objs {
            println!("\t.field public {} '{}'", value.borrow().ty.borrow().to_ilstr(), value.borrow().name);
        }
        for im in &st.borrow().impls {
            for func in &im.functions {
                if let Some(nested_class) = &func.nested_class {
                    println!(".class nested private auto ansi sealed beforefieldinit '{}' extends [System.Runtime]System.Object {{", nested_class.borrow().name);
                    for value in &nested_class.borrow().field.objs {
                        println!("\t.field public {} '{}'", value.borrow().ty.borrow().to_ilstr(), value.borrow().name);
                    }
                    if nested_class.borrow().name == "<>c__DisplayClass0_0" {
                        println!("\t.method public hidebysig specialname rtspecialname instance void .ctor() cil managed {{");
                        println!("\t\tldarg.0");
                        println!("\t\tcall instance void [System.Runtime]System.Object::.ctor()");
                        println!("\t\tret");
                        println!("\t}}");
                    }
                    for local_func in &func.local_funcs {
                        gen_local_function(program, local_func);
                    }
                    println!("}}");
                }
                let args = func
                    .param_symbol_table
                    .borrow()
                    .objs
                    .iter()
                    .skip(if func.is_static { 0 } else { 1 })
                    .map(|o|format!("{} '{}'", o.borrow().ty.borrow().to_ilstr(), o.borrow().name))
                    .collect::<Vec<String>>()
                    .join(", ");
                println!("\t.method public {} {} '{}'({}) cil managed {{",
                    if func.is_static {"static"} else {"instance"},
                    func.rettype.borrow().to_ilstr(),
                    func.name,
                    args);
                println!("\t\t.maxstack 32");

                if let Ok(rettype) = codegen::gen_il(func.statements.clone(), program) {
                    match (&rettype, &*func.rettype.borrow()) {
                        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                        _ => if rettype != *func.rettype.borrow() {
                            panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.borrow(), rettype);
                        }
                    }
                }
                program.push_il("\t\tret");

                // prepare local variables
                let locals = func
                    .lvar_symbol_table
                    .borrow()
                    .objs
                    .iter()
                    .enumerate()
                    //.map(|(i, obj)| format!("\t\t\t{} V_{}", obj.borrow().ty.borrow().to_ilstr(), i))
                    .map(|(_, obj)| format!("\t\t\t{} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
                    .collect::<Vec<String>>()
                    .join(",\n");
                if !locals.is_empty() {
                    println!("\t\t.locals init (");
                    println!("{}", locals);
                    println!("\t\t)");
                }

                program.display_il();

                println!("\t}}");
            }
        }
        println!("}}");
    }
}

fn gen_functions<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    println!(".class private auto ansi abstract sealed beforefieldinit '{}' extends [System.Runtime]System.Object {{", program.name);
    for func in &namespace.functions {
        gen_function(program, func);
        if let Some(nested_class) = &func.nested_class {
            println!(".class nested private auto ansi sealed beforefieldinit '{}' extends [System.Runtime]System.Object {{", nested_class.borrow().name);
            for value in &nested_class.borrow().field.objs {
                println!("\t.field public {} '{}'", value.borrow().ty.borrow().to_ilstr(), value.borrow().name);
            }
            if nested_class.borrow().name == "<>c__DisplayClass0_0" {
                println!("\t.method public hidebysig specialname rtspecialname instance void .ctor() cil managed {{");
                println!("\t\tldarg.0");
                println!("\t\tcall instance void [System.Runtime]System.Object::.ctor()");
                println!("\t\tret");
                println!("\t}}");
            }
            for local_func in &func.local_funcs {
                gen_local_function(program, local_func);
            }
            println!("}}");
        }
    }
    println!("}}");
}

fn gen_local_function<'a, 'b>(program: &'a Program<'a>, func: &'b Function<'a>) {
    let args = func
        .param_symbol_table
        .borrow()
        .objs
        .iter()
        .map(|o|format!("{} '{}'", o.borrow().ty.borrow().to_ilstr(), o.borrow().name))
        .collect::<Vec<String>>()
        .join(", ");
    println!("\t.method assembly instance {} '{}'({}) cil managed {{", func.rettype.borrow().to_ilstr(), func.name, args);
    println!("\t\t.maxstack 32");

    if let Ok(rettype) = codegen::gen_il(func.statements.clone(), program) {
        match (&rettype, &*func.rettype.borrow()) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
            _ => if rettype != *func.rettype.borrow() {
                panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.borrow(), rettype);
            }
        }
    }
    program.push_il("\t\tret");

    // prepare local variables
    let locals = func
        .lvar_symbol_table
        .borrow()
        .objs
        .iter()
        .enumerate()
        //.map(|(i, obj)| format!("\t\t\t{} V_{}", obj.borrow().ty.borrow().to_ilstr(), i))
        .map(|(_, obj)| format!("\t\t\t{} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
        .collect::<Vec<String>>()
        .join(",\n");
    if !locals.is_empty() {
        println!("\t\t.locals init (");
        println!("{}", locals);
        println!("\t\t)");
    }

    program.display_il();

    println!("}}");
}

fn gen_function<'a, 'b>(program: &'a Program<'a>, func: &'b Function<'a>) {
    if func.name == "main" {
        println!(".method static void Main() cil managed {{");
        println!("\t.entrypoint");
    } else {
        let args = func
            .param_symbol_table
            .borrow()
            .objs
            .iter()
            .map(|o|format!("{} '{}'", o.borrow().ty.borrow().to_ilstr(), o.borrow().name))
            .collect::<Vec<String>>()
            .join(", ");
        println!(".method static {} '{}'({}) cil managed {{", func.rettype.borrow().to_ilstr(), func.name, args);
    }
    println!("\t.maxstack 32");

    if let Ok(rettype) = codegen::gen_il(func.statements.clone(), program) {
        match (&rettype, &*func.rettype.borrow()) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
            _ => if rettype != *func.rettype.borrow() {
                panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.borrow(), rettype);
            }
        }
    }
    program.push_il("\tret");

    // prepare local variables
    let locals = func
        .lvar_symbol_table
        .borrow()
        .objs
        .iter()
        .enumerate()
        //.map(|(i, obj)| format!("\t\t{} V_{}", obj.borrow().ty.borrow().to_ilstr(), i))
        .map(|(_, obj)| format!("\t\t{} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
        .collect::<Vec<String>>()
        .join(",\n");
    if !locals.is_empty() {
        println!("\t.locals init (");
        println!("{}", locals);
        println!("\t)");
        func.lvar_symbol_table
            .borrow()
            .objs
            .iter()
            .for_each(|obj| if let Type::Class(ClassKind::NestedClass(pn), .., name, _, _) = &*obj.borrow().ty.borrow() {
                if name == "<>c__DisplayClass0_0" {
                    println!("\tnewobj instance void '{}'/'<>c__DisplayClass0_0'::.ctor()", pn);
                    println!("\tstloc '{}'", obj.borrow().name);
                }
            });
    }

    program.display_il();

    println!("}}");
}

fn disp_errors(program: &Program) {
    let err_count = program.errors.borrow().err_count();
    program.errors.borrow().display();
    eprintln!("\x1b[31merror\x1b[0m: could not compile due to {} previous errors", err_count);
    std::process::exit(1);
}
