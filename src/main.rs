mod token;
mod lexer;
mod ast;
mod parser;

fn main() {
    let input = std::env::args().nth(1).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    let code_ast = ast::gen_ast(&tokens);

    println!(".assembly tmp {{}}");

    println!(".method static int32 Main() cil managed {{");
    println!(".entrypoint");

    for code in code_ast {
        parser::gen_il(*code);
        println!("pop");
    }

    println!("ret");
    println!("}}");
}
