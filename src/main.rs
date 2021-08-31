mod token;
mod lexer;
mod ast;
mod parser;

fn main() {
    let input = " 1 + 2 * 3 ";

    let mut lexer = lexer::Lexer::new(input);
    let tokens = lexer::tokenize(&mut lexer);
    let ast = ast::gen_ast(&tokens);

    let result = parser::eval(&ast);
    println!("{} = {}", input, result);
}
