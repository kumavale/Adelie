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

use crate::keyword::*;

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let input = std::fs::read_to_string(&path).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    //eprintln!("{:?}", tokens.iter().map(|t|t.kind.clone()).collect::<Vec<token::TokenKind>>());
    //eprintln!("{:?}", tokens);

    let mut g_symbol_table = object::SymbolTable::new();
    let mut parser = parser::Parser::new(&path, &input, &tokens, &mut g_symbol_table);
    let program = parser.gen_ast();

    println!(".assembly extern mscorlib {{}}");
    println!(".assembly tmp {{}}");

    for st in &program.structs {
        println!(".class private sequential auto sealed beforefieldinit {} extends System.ValueType", st.name);
        println!("{{");
        for value in &st.field {
            println!("\t.field public {} {}", value.ty.to_ilstr(), value.name);
        }
        println!("}}");
    }

    for im in &program.impls {
        println!(".class private sequential auto sealed beforefieldinit {} extends System.ValueType", im.name);
        println!("{{");
        for func in &im.functions {
            let args = func
                .param_symbol_table
                .objs
                .iter()
                .skip(if func.is_static { 0 } else { 1 })
                .map(|o|format!("{} {}", o.ty.to_ilstr(), o.name))
                .collect::<Vec<String>>()
                .join(", ");
            println!("\t.method public {} {} {}({}) cil managed {{",
                if func.is_static {"static"} else {"instance"},
                func.rettype.to_ilstr(),
                func.name,
                args);
            println!("\t\t.maxstack 32");

            // prepare local variables
            println!("\t\t.locals init (");
            let locals = func
                .lvar_symbol_table
                .objs
                .iter()
                .enumerate()
                .map(|(i, obj)| {
                    if let keyword::Type::Struct(name) = &obj.ty{
                        use crate::object::FindSymbol;
                        if program.structs.find(name).is_none() {
                            panic!("cannot find struct, variant or union type `{}` in this scope", name);
                        }
                    }
                    format!("\t\t\t{} V_{}", obj.ty.to_ilstr(), i)
                })
                .collect::<Vec<String>>()
                .join(",\n");
            println!("\t\t{})", locals);

            let rettype = codegen::gen_il(func.statements.clone(), &program);
            match (&rettype, &func.rettype) {
                (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                _ => if rettype != func.rettype {
                    panic!("{}: expected `{}`, found `{}`", func.name, func.rettype, rettype);
                }
            }

            println!("\t\tret");
            println!("\t}}");
        }
        println!("}}");
    }

    for func in &program.functions {
        if func.name == "main" {
            println!(".method static void Main() cil managed {{");
            println!("\t.entrypoint");
        } else {
            let args = func
                .param_symbol_table
                .objs
                .iter()
                .map(|o|format!("{} {}", o.ty.to_ilstr(), o.name))
                .collect::<Vec<String>>()
                .join(", ");
            println!(".method static {} {}({}) cil managed {{", func.rettype.to_ilstr(), func.name, args);
        }
        println!("\t.maxstack 32");

        // prepare local variables
        println!("\t.locals init (");
        let locals = func
            .lvar_symbol_table
            .objs
            .iter()
            .enumerate()
            .map(|(i, obj)| {
                if let keyword::Type::Struct(name) = &obj.ty{
                    use crate::object::FindSymbol;
                    if program.structs.find(name).is_none() {
                        panic!("cannot find struct, variant or union type `{}` in this scope", name);
                    }
                }
                format!("\t\t{} V_{}", obj.ty.to_ilstr(), i)
            })
            .collect::<Vec<String>>()
            .join(",\n");
        println!("\t{})", locals);

        let rettype = codegen::gen_il(func.statements.clone(), &program);
        match (&rettype, &func.rettype) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
            _ => if rettype != func.rettype {
                panic!("{}: expected `{}`, found `{}`", func.name, func.rettype, rettype);
            }
        }

        println!("\tret");
        println!("}}");
    }
}
