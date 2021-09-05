mod token;
mod lexer;
mod ast;
mod parser;
mod codegen;
mod object;
mod function;

fn main() {
    let input = std::env::args().nth(1).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    //eprintln!("{:?}", tokens);

    let mut fn_symbol_table  = object::SymbolTable::new();
    let code_ast = parser::gen_ast(&tokens, &mut fn_symbol_table);

    println!(".assembly tmp {{}}");

    for func in code_ast {
        if func.name == "main" {
            println!(".method static int32 Main() cil managed {{");
            println!("\t.entrypoint");
        } else {
            let args = func.param_symbol_table.objs.iter().map(|o|format!("int32 {}", o.name)).collect::<Vec<String>>().join(", ");
            println!(".method static int32 {}({}) cil managed {{", func.name, args);
        }

        // prepare local variables
        println!("\t.locals init (");
        for i in 0..func.lvar_symbol_table.len() {
            println!("\t\t[{}] int32 V_{}{}", i, i, if i+1<func.lvar_symbol_table.len(){","}else{""});
        }
        println!("\t)");

        if let Some(statements) = func.statements {
            codegen::gen_il(statements);
        }

        println!("\tret");
        println!("}}");
    }
}
