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
use crate::object::ObjectKind;
use crate::namespace::NameSpace;
use crate::program::{Program, IlEnum, IlManifest};
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
    let mut ilman = IlManifest::new(&program.name);

    // 内部で使用
    ilman.push_asm("mscorlib", None);

    // externブロックで使用
    for assembly in &program.references {
        let name = assembly.find_value("name").unwrap();
        ilman.push_asm(name, assembly.find_value("publickeytoken"));
    }

    program.push_il_mani(ilman);

    // ひとまず直ぐに出力
    program.display_il();
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
    gen_enums(program, namespace);
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
        println!(".class private sequential auto sealed beforefieldinit '{}' extends [mscorlib]System.ValueType", st.borrow().name);
        println!("{{");
        for value in &st.borrow().field.objs {
            println!("\t.field public {} '{}'", value.borrow().ty.borrow().to_ilstr(), value.borrow().name);
        }
        for im in &st.borrow().impls {
            for func in &im.functions {
                if let Some(nested_class) = &func.nested_class {
                    println!(".class nested private auto ansi sealed beforefieldinit '{}' extends [mscorlib]System.Object {{", nested_class.borrow().name);
                    for value in &nested_class.borrow().field.objs {
                        println!("\t.field public {} '{}'", value.borrow().ty.borrow().to_ilstr(), value.borrow().name);
                    }
                    if nested_class.borrow().name == "<>c__DisplayClass0_0" {
                        println!("\t.method public hidebysig specialname rtspecialname instance void .ctor() cil managed {{");
                        println!("\t\tldarg.0");
                        println!("\t\tcall instance void [mscorlib]System.Object::.ctor()");
                        println!("\t\tret");
                        println!("\t}}");
                    }
                    for local_func in &func.local_funcs {
                        gen_method(program, local_func);
                    }
                    println!("}}");
                }
                gen_method(program, func);
            }
        }
        println!("}}");
    }
}

fn gen_enums<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for ed in &namespace.enums {
        let mut ilenum = IlEnum::new(&ed.name);
        ilenum.push_field(".field public specialname rtspecialname int32 '<>value__'");
        for enumobj in &ed.fields {
            ilenum.push_field(format!(".field public static literal valuetype '{}' '{}' = int32({})", ed.name, enumobj.name, enumobj.value));
        }
        program.push_il_enum(ilenum);
    }
    // ひとまず直ぐに出力
    program.display_il();
}

fn gen_functions<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    println!(".class private auto ansi abstract sealed beforefieldinit '{}' extends [mscorlib]System.Object {{", program.name);
    for func in &namespace.functions {
        gen_function(program, func);
        if let Some(nested_class) = &func.nested_class {
            println!(".class nested private auto ansi sealed beforefieldinit '{}' extends [mscorlib]System.Object {{", nested_class.borrow().name);
            for value in &nested_class.borrow().field.objs {
                println!("\t.field public {} '{}'", value.borrow().ty.borrow().to_ilstr(), value.borrow().name);
            }
            if nested_class.borrow().name == "<>c__DisplayClass0_0" {
                println!("\t.method public hidebysig specialname rtspecialname instance void .ctor() cil managed {{");
                println!("\t\tldarg.0");
                println!("\t\tcall instance void [mscorlib]System.Object::.ctor()");
                println!("\t\tret");
                println!("\t}}");
            }
            for local_func in &func.local_funcs {
                gen_method(program, local_func);
            }
            println!("}}");
        }
    }
    println!("}}");
}

fn gen_method<'a, 'b>(program: &'a Program<'a>, func: &'b Function<'a>) {
    let args = func
        .symbol_table
        .borrow()
        .objs
        .iter()
        .filter(|o| o.borrow().kind == ObjectKind::Param)
        .skip((!func.is_static) as usize)
        .map(|o|format!("{} '{}'", o.borrow().ty.borrow().to_ilstr(), o.borrow().name))
        .collect::<Vec<String>>()
        .join(", ");
    println!("\t.method assembly {} {} '{}'({}) cil managed {{",
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
    program.push_il_text("\t\tret");

    // prepare local variables
    let locals = func
        .symbol_table
        .borrow()
        .objs
        .iter()
        .filter(|o| o.borrow().kind == ObjectKind::Local)
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
            .symbol_table
            .borrow()
            .objs
            .iter()
            .filter(|o| o.borrow().kind == ObjectKind::Param)
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
                panic!("{}: expected `{:?}`, found `{:?}`", func.name, func.rettype.borrow(), rettype);
            }
        }
    }
    program.push_il_text("\tret");

    // prepare local variables
    let locals = func
        .symbol_table
        .borrow()
        .objs
        .iter()
        .filter(|o| o.borrow().kind == ObjectKind::Local)
        .enumerate()
        //.map(|(i, obj)| format!("\t\t{} V_{}", obj.borrow().ty.borrow().to_ilstr(), i))
        .map(|(_, obj)| format!("\t\t{} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
        .collect::<Vec<String>>()
        .join(",\n");
    if !locals.is_empty() {
        println!("\t.locals init (");
        println!("{}", locals);
        println!("\t)");
        func.symbol_table
            .borrow()
            .objs
            .iter()
            .filter(|o| o.borrow().kind == ObjectKind::Local)
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
