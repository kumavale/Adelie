mod token;
mod lexer;
mod ast;
mod parser;
mod codegen;
mod object;

fn main() {
    let input = std::env::args().nth(1).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    let mut symbol_table = object::SymbolTable::new();
    let code_ast = parser::gen_ast(&tokens, &mut symbol_table);

    println!(".assembly tmp {{}}");

    println!(".method static int32 Main() cil managed {{");
    println!(".entrypoint");

    // prepare local variables
    println!(".locals init (");
    for i in 0..symbol_table.len() {
        println!("[{}] int32 V_{}{}", i, i, if i+1<symbol_table.len(){","}else{""});
    }
    println!(")");

    for code in code_ast {
        codegen::gen_il(*code);
        println!("pop");
    }

    println!("ret");
    println!("}}");
}
