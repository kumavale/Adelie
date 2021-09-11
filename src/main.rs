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
    //eprintln!("{:?}", tokens.iter().map(|t|t.kind.clone()).collect::<Vec<token::TokenKind>>());
    //eprintln!("{:?}", tokens);

    let mut fn_symbol_table = object::SymbolTable::new();
    let code_ast = parser::gen_ast(&tokens, &mut fn_symbol_table);

    println!(".assembly extern mscorlib {{}}");
    println!(".assembly tmp {{}}");

    for func in &code_ast {
        if func.name == "main" {
            println!(".method static void Main() cil managed {{");
            println!("\t.entrypoint");
        } else {
            let args = func.param_symbol_table.objs.iter().map(|o|format!("{} {}", o.typekind.to_ilstr(), o.name)).collect::<Vec<String>>().join(", ");
            println!(".method static {} {}({}) cil managed {{", func.rettype.to_ilstr(), func.name, args);
        }
        println!("\t.maxstack 32");

        // prepare local variables
        println!("\t.locals init (");
        for (i, obj) in func.lvar_symbol_table.objs.iter().enumerate() {
            println!("\t\t[{}] {} V_{}{}", i, obj.typekind.to_ilstr(), i, if i+1<func.lvar_symbol_table.len(){","}else{""});
        }
        println!("\t)");

        let rettype = codegen::gen_il(func.statements.clone(), &code_ast);
        if rettype != func.rettype {
            panic!("{}: expected `{}`, found `{}`", func.name, func.rettype.to_str(), rettype.to_str());
        }

        println!("\tret");
        println!("}}");
    }
}
