mod ast;
mod builtin;
mod class;
mod codegen;
mod error;
mod function;
mod keyword;
mod lexer;
mod namespace;
mod object;
mod parser;
mod program;
mod token;
mod utils;

use crate::class::{Class, ClassKind};
use crate::error::Errors;
use crate::function::Function;
use crate::keyword::{Type, RRType, Numeric};
use crate::object::ObjectKind;
use crate::namespace::NameSpace;
use crate::program::{Program, IlEnum, IlManifest, IlFunc, IlClass};
use std::cell::RefCell;
use std::io::{self, Write};
use std::path::Path;
use std::process::Command;
use std::rc::Rc;

fn main() {
    // 引数解析
    let path = std::env::args().nth(1).unwrap();


    // コンパイル: Adelie to il
    println!(" Compiling '{}'", &path);
    let input = std::fs::read_to_string(&path).unwrap();

    let mut lexer = lexer::Lexer::new(&input);
    let tokens = lexer::tokenize(&mut lexer);
    //eprintln!("{:?}", tokens.iter().map(|t|t.kind.clone()).collect::<Vec<token::TokenKind>>());
    //eprintln!("{:?}", tokens);

    let errors = Rc::new(RefCell::new(Errors::new()));
    let mut g_symbol_table = object::SymbolTable::new();
    let mut parser = parser::Parser::new(&path, &input, &tokens, &mut g_symbol_table, errors);
    let program = parser.gen_ast();

    // パース段階のエラーを表示
    if !program.errors.borrow().is_empty() {
        disp_errors(&program);
    }

    // il生成
    gen_manifest(&program);
    gen_items(&program, &program.namespace.borrow());

    // il生成段階のエラーを表示
    if !program.errors.borrow().is_empty() {
        disp_errors(&program);
    }

    // ilをファイルに出力
    program.write_il().unwrap();


    // コンパイル: il to exe
    let ilasm = if cfg!(windows) {
        format!("{}/Microsoft.NET/Framework64/v4.0.30319/ilasm.exe", std::env::var("SYSTEMROOT").unwrap())
    } else if cfg!(unix) {
        "ilasm".to_string()
    } else {
        unimplemented!();
    };
    let infile = Path::new(&path)
        .with_extension("il");
    let outfile = Path::new(&path)
        .with_extension("exe");
    let output = Command::new(ilasm)
        .args([&format!("/OUTPUT={}", outfile.to_string_lossy()), &infile.to_string_lossy().to_string()])
        .output()
        .unwrap();
    let _ = io::stdout().write_all(&output.stdout);
    let _ = io::stderr().write_all(&output.stderr);
}

fn gen_manifest<'a>(program: &'a Program<'a>) {
    let mut ilman = IlManifest::new(&program.name);

    // 内部で使用
    ilman.push_asm("mscorlib", None);
    ilman.push_asm("adelie_std", None);

    // externブロックで使用
    for assembly in &program.references {
        let name = assembly.find_value("name").unwrap();
        ilman.push_asm(&name[..name.len()-4], assembly.find_value("publickeytoken"));
    }

    program.push_il_mani(ilman);
}

fn gen_items<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    if namespace.is_foreign {
        return;
    }
    gen_structs(program, namespace);
    gen_enums(program, namespace);
    gen_functions(program, namespace);
    for child in &namespace.children {
        gen_items(program, &child.borrow());
    }
}

fn gen_structs<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for st in &namespace.classes {
        let fields = st.borrow()
            .field
            .objs
            .iter()
            .map(|obj| format!("\t.field public {} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
            .collect();
        for im in &st.borrow().impls {
            for func in &im.functions {
                if let Some(nested_class) = &func.nested_class {
                    gen_nested_class(program, nested_class, func);
                }
                gen_function(program, func);
            }
        }
        let funcs  = program.drain_il_funcs();
        let nested = program.drain_il_nested_classes();
        let ilclass = IlClass::new(&st.borrow().name, ClassKind::Struct, fields, funcs, nested);
        program.push_il_class(ilclass);
    }
}

fn gen_nested_class<'a, 'b>(program: &'a Program<'a>, nested_class: &'b Rc<RefCell<Class>>, func: &'b Function<'a>) {
    let fields = nested_class.borrow()
        .field
        .objs
        .iter()
        .map(|obj| format!("\t.field public {} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
        .collect();
    for local_func in &func.local_funcs {
        gen_function(program, local_func);
    }
    let funcs = program.drain_il_funcs();
    let ilclass = IlClass::new(&nested_class.borrow().name, nested_class.borrow().kind.clone(), fields, funcs, vec![]);

    program.push_il_class(ilclass);
}

fn gen_enums<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for ed in &namespace.enums {
        let mut ilenum = IlEnum::new(&ed.name);
        ilenum.push_field(".field public specialname rtspecialname int32 '<>value__'");
        for enumobj in &ed.fields {
            ilenum.push_field(format!(".field public static literal valuetype '{}' '{}' = int32({})", ed.name, enumobj.name, enumobj.value));
        }
        program.push_il_enum(ilenum);
    }
}

fn gen_functions<'a, 'b>(program: &'a Program<'a>, namespace: &'b NameSpace<'a>) {
    for func in &namespace.functions {
        if let Some(nested_class) = &func.nested_class {
            gen_nested_class(program, nested_class, func);
        }
        gen_function(program, func);
    }
    let funcs  = program.drain_il_funcs();
    let nested = program.drain_il_nested_classes();
    let ilclass = IlClass::new(&program.name, ClassKind::Class, vec![], funcs, nested);
    program.push_il_class(ilclass);
}

fn gen_function<'a, 'b>(program: &'a Program<'a>, func: &'b Function<'a>) {
    if let Ok(rettype) = codegen::gen_il(func.statements.clone(), program) {
        match (&rettype, &*func.rettype.borrow()) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
            _ => if rettype != *func.rettype.borrow() {
                if let ast::NodeKind::Block { stmts } = &func.statements.kind {
                    let token = if stmts.is_empty() {
                        func.statements.token
                    } else {
                        stmts.last().unwrap().token
                    };
                    error::e0012(Rc::clone(&program.errors), (program.path, &program.lines, token), &func.rettype.borrow(), &rettype);
                } else {
                    unreachable!();
                }
            }
        }
    }
    let params = func
            .symbol_table
            .borrow()
            .objs
            .iter()
            .filter(|obj| obj.borrow().kind == ObjectKind::Param)
            .skip((!func.is_static) as usize)
            .map(|obj|format!("{} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
            .collect::<Vec<String>>()
            .join(", ");
    let locals = func
        .symbol_table
        .borrow()
        .objs
        .iter()
        .filter(|obj| obj.borrow().kind == ObjectKind::Local)
        .map(|obj| format!("\t\t{} '{}'", obj.borrow().ty.borrow().to_ilstr(), obj.borrow().name))
        .collect::<Vec<String>>()
        .join(",\n");
    let inits = func.symbol_table
        .borrow()
        .objs
        .iter()
        .filter(|obj| obj.borrow().kind == ObjectKind::Local)
        .filter(|obj| matches!(&*obj.borrow().ty.borrow(), Type::Class(ClassKind::NestedClass(_), ..)))
        .map(Rc::clone)
        .collect::<Vec<_>>();
    let stmts = program.drain_il_stmts();

    let ilfunc = IlFunc::new(&func.name, func.is_static, RRType::clone(&func.rettype), params, locals, inits, stmts);
    program.push_il_func(ilfunc);
}

fn disp_errors(program: &Program) {
    let err_count = program.errors.borrow().err_count();
    program.errors.borrow().display();
    eprintln!("\x1b[31merror\x1b[0m: could not compile due to {} previous errors", err_count);
    std::process::exit(1);
}
