mod token;
mod keyword;
mod lexer;
mod ast;
mod parser;
mod codegen;
mod object;
mod function;
mod builtin;
mod class;
mod program;
mod utils;
mod error;

fn main() {
    let input = std::env::args().nth(1).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    //eprintln!("{:?}", tokens.iter().map(|t|t.kind.clone()).collect::<Vec<token::TokenKind>>());
    //eprintln!("{:?}", tokens);

    let mut g_symbol_table = object::SymbolTable::new();
    let program = parser::gen_ast(&input, &tokens, &mut g_symbol_table);

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
            println!("\t.method public instance {} {}({}) cil managed {{", func.rettype.to_ilstr(), func.name, args);
            println!("\t\t.maxstack 32");

            // prepare local variables
            println!("\t\t.locals init (");
            for (i, obj) in func.lvar_symbol_table.objs.iter().enumerate() {
                if let keyword::Type::Struct(name) = &obj.ty{
                    use crate::object::FindSymbol;
                    if program.structs.find(name).is_none() {
                        panic!("cannot find struct, variant or union type `{}` in this scope", name);
                    }
                }
                println!("\t\t\t[{}] {} V_{}{}", i, obj.ty.to_ilstr(), i, if i+1<func.lvar_symbol_table.len(){","}else{""});
            }
            println!("\t\t)");

            let rettype = codegen::gen_il(func.statements.clone(), &program);
            if rettype != func.rettype {
                panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.to_string(), rettype.to_string());
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
        for (i, obj) in func.lvar_symbol_table.objs.iter().enumerate() {
            if let keyword::Type::Struct(name) = &obj.ty {
                use crate::object::FindSymbol;
                if program.structs.find(name).is_none() {
                    panic!("cannot find struct, variant or union type `{}` in this scope", name);
                }
            }
            println!("\t\t[{}] {} V_{}{}", i, obj.ty.to_ilstr(), i, if i+1<func.lvar_symbol_table.len(){","}else{""});
        }
        println!("\t)");

        let rettype = codegen::gen_il(func.statements.clone(), &program);
        if rettype != func.rettype {
            panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.to_string(), rettype.to_string());
        }

        println!("\tret");
        println!("}}");
    }
}
