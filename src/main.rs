mod token;
mod keyword;
mod lexer;
mod ast;
mod parser;
mod codegen;
mod object;
mod function;
mod builtin;

fn main() {
    let input = std::env::args().nth(1).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    //eprintln!("{:?}", tokens);

    let mut fn_symbol_table = object::SymbolTable::new();
    let code_ast = parser::gen_ast(&tokens, &mut fn_symbol_table);
    let fst = function::FunctionSymbolTable::new(&code_ast);

    println!(".assembly extern mscorlib {{}}");
    println!(".assembly tmp {{}}");

    for func in code_ast {
        if func.name == "main" {
            println!(".method static int32 Main() cil managed {{");
            println!("\t.entrypoint");
        } else {
            let args = func.param_symbol_table.objs.iter().map(|o|format!("{} {}", o.typekind.as_ilstr(), o.name)).collect::<Vec<String>>().join(", ");
            println!(".method static int32 {}({}) cil managed {{", func.name, args);
        }
        println!("\t.maxstack 32");

        // prepare local variables
        println!("\t.locals init (");
        for (i, obj) in func.lvar_symbol_table.objs.iter().enumerate() {
            println!("\t\t[{}] {} V_{}{}", i, obj.typekind.as_ilstr(), i, if i+1<func.lvar_symbol_table.len(){","}else{""});
        }
        println!("\t)");

        if let Some(statements) = func.statements {
            codegen::gen_il(statements, &fst);
        }

        println!("\tret");
        println!("}}");
    }
}
