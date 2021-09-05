use super::ast::*;
use super::builtin::*;
use super::function::*;

pub fn gen_il(node: Node, fst: &FunctionSymbolTable) {
    match node {
        Node::Integer { typekind, num } => println!("\tldc.i4 {}", num as i32),
        Node::String { typekind, str } => println!("\tldstr \"{}\"", str),
        Node::Builtin { kind, args } => gen_builtin_il(kind, args, fst),
        Node::Function { obj, args } => {
            for arg in args {
                gen_il(*arg, fst);
            }
            let args = fst.params(&obj.name).unwrap().objs.iter().map(|o|o.typekind.as_ilstr()).collect::<Vec<&str>>().join(", ");
            println!("\tcall int32 {}({})", obj.name, args);
        }
        Node::Variable { typekind, obj } => {
            if obj.is_param {
                println!("\tldarg {}", obj.offset);
            } else {
                println!("\tldloc {}", obj.offset);
            }
        }
        Node::Block { stmts } => {
            for stmt in stmts {
                gen_il(*stmt, fst);
            }
        }
        Node::If { cond, then, els } => {
            static mut SEQ: usize = 0;
            gen_il(*cond, fst);
            let else_label = format!("IL_else{}", unsafe { SEQ });
            let end_label = format!("IL_end{}", unsafe { SEQ });
            unsafe { SEQ += 1; }
            println!("\tbrfalse {}", else_label);
            gen_il(*then, fst);
            println!("\tbr {}", end_label);
            println!("{}:", else_label);
            if let Some(els) = els {
                gen_il(*els, fst);
            }
            println!("{}:", end_label);
        }
        Node::While { cond, then } => {
            static mut SEQ: usize = 0;
            let begin_label = format!("IL_begin{}", unsafe { SEQ });
            let end_label = format!("IL_end{}", unsafe { SEQ });
            unsafe { SEQ += 1; }
            println!("{}:", begin_label);
            gen_il(*cond, fst);
            println!("\tbrfalse {}", end_label);
            gen_il(*then, fst);
            println!("\tbr {}", begin_label);
            println!("{}:", end_label);
        }
        Node::Assign { lhs, rhs } => {
            if let Node::Variable { typekind, obj } = *lhs {
                gen_il(*rhs, fst);
                println!("\tstloc {}", obj.offset);
                println!("\tldc.i4.0");
            } else {
                panic!("The left-hand side of an assignment must be a variable");
            }
        }
        Node::Pop { expr } => {
            gen_il(*expr, fst);
            println!("\tpop");
        }
        Node::Return { expr } => {
            gen_il(*expr, fst);
            println!("\tret");
        }
        Node::UnaryOp { kind: _, expr } => {
            gen_il(*expr, fst);
            println!("\tneg");
        }
        Node::BinaryOp { kind, lhs, rhs } => {
            gen_il(*lhs, fst);
            gen_il(*rhs, fst);
            match kind {
                BinaryOpKind::Add => println!("\tadd"),
                BinaryOpKind::Sub => println!("\tsub"),
                BinaryOpKind::Mul => println!("\tmul"),
                BinaryOpKind::Div => println!("\tdiv"),
                BinaryOpKind::Rem => println!("\trem"),

                BinaryOpKind::Eq => println!("\tceq"),
                BinaryOpKind::Lt => println!("\tclt"),
                BinaryOpKind::Le => {
                    println!("\tcgt");
                    println!("\tldc.i4.0");
                    println!("\tceq");
                }
                BinaryOpKind::Ne => {
                    println!("\tceq");
                    println!("\tldc.i4.0");
                    println!("\tceq");
                }
                BinaryOpKind::Gt => println!("\tcgt"),
                BinaryOpKind::Ge => {
                    println!("\tclt");
                    println!("\tldc.i4.0");
                    println!("\tceq");
                }
            }
        }
    }
}
