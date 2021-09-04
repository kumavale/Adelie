mod token;
mod lexer;
mod ast;
mod parser;
mod object;

fn main() {
    let input = std::env::args().nth(1).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    let symbol_table = object::SymbolTable::new();
    let code_ast = ast::gen_ast(&tokens);

    println!(".assembly tmp {{}}");

    println!(".method static int32 Main() cil managed {{");
    println!(".entrypoint");

    // prepare local variables
    println!(".locals init (");
    for i in 0..25 {
        println!("[{}] int32 V_{},", i, i);
    }
    println!("[25] int32 V_25");
    println!(")");

    for code in code_ast {
        parser::gen_il(*code);
        println!("pop");
    }

    println!("ret");
    println!("}}");
}
