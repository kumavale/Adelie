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

    // TEST ===================================================================
    println!(".method private hidebysig static int32 plus(int32 a, int32 b) cil managed {{");
    println!("\t.locals init (");
    println!("\t\t[0] int32 V_0");
    println!("\t)");
    println!("\tnop");
    println!("\tldarg.0");
    println!("\tldarg.1");
    println!("\tadd");
    println!("\tstloc.0");
    println!("\tldloc.0");
    println!("\tret");
    println!("}}");
    // ========================================================================

    for func in code_ast {
        if func.name == "main" {
            println!(".method static int32 Main() cil managed {{");
            println!("\t.entrypoint");
        } else {
            println!(".method static int32 {}() cil managed {{", func.name);
        }

        // prepare local variables
        println!("\t.locals init (");
        for i in 0..func.symbol_table.len() {
            println!("\t\t[{}] int32 V_{}{}", i, i, if i+1<func.symbol_table.len(){","}else{""});
        }
        println!("\t)");

        if let Some(statements) = func.statements {
            codegen::gen_il(statements);
        }

        println!("\tret");
        println!("}}");
    }
}
