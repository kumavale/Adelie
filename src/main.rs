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

    // TEST ===================================================================
    println!(".method private hidebysig static int32 ret3() cil managed {{");
    println!("\t.locals init (");
    println!("\t\t[0] int32 V_0");
    println!("\t)");
    println!("\tnop");
    println!("\tldc.i4.3");
    println!("\tstloc.0");
    println!("\tldloc.0");
    println!("\tret");
    println!("}}");
    // ========================================================================

    println!(".method static int32 Main() cil managed {{");
    println!("\t.entrypoint");

    // prepare local variables
    println!("\t.locals init (");
    for i in 0..symbol_table.len() {
        println!("\t\t[{}] int32 V_{}{}", i, i, if i+1<symbol_table.len(){","}else{""});
    }
    println!("\t)");

    for code in code_ast {
        codegen::gen_il(*code);
    }

    println!("\tret");
    println!("}}");
}
