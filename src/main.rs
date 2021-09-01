mod token;
mod lexer;
mod ast;
mod parser;

fn main() {
    let input = " 1 + 2 * 3 ";

    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer::tokenize(&mut lexer);
    let ast = ast::gen_ast(&tokens);

    println!(".assembly test {{}}");

    println!(".method static int32 Main() cil managed {{");
    println!(".entrypoint");

    parser::gen_il(&ast);
    //let result = parser::eval(&ast);
    //println!("{} = {}", input, result);

    println!("ret");
    println!("}}");
}
