use super::ast::*;

pub fn gen_il(node: Node) {
    match node {
        Node::Integer(n) => println!("\tldc.i4 {}", n as i32),
        Node::Function { obj, args } => {
            let argc = args.len();
            for arg in args {
                gen_il(*arg);
            }
            print!("\tcall int32 {}(", obj.name);
            for i in 0..argc {
                print!("int32{}", if i+1<argc{","}else{""});
            }
            println!(")");
        }
        Node::Variable(obj) => {
            println!("\tldloc {}", obj.offset);
        }
        Node::Block { stmts } => {
            for stmt in stmts {
                gen_il(*stmt);
            }
        }
        Node::If { cond, then, els } => {
            static mut SEQ: usize = 0;
            gen_il(*cond);
            let else_label = format!("IL_else{}", unsafe { SEQ });
            let end_label = format!("IL_end{}", unsafe { SEQ });
            unsafe { SEQ += 1; }
            println!("\tbrfalse {}", else_label);
            gen_il(*then);
            println!("\tbr {}", end_label);
            println!("{}:", else_label);
            if let Some(els) = els {
                gen_il(*els);
            }
            println!("{}:", end_label);
        }
        Node::While { cond, then } => {
            static mut SEQ: usize = 0;
            let begin_label = format!("IL_begin{}", unsafe { SEQ });
            let end_label = format!("IL_end{}", unsafe { SEQ });
            unsafe { SEQ += 1; }
            println!("{}:", begin_label);
            gen_il(*cond);
            println!("\tbrfalse {}", end_label);
            gen_il(*then);
            println!("\tbr {}", begin_label);
            println!("{}:", end_label);
        }
        Node::Assign { lhs, rhs } => {
            if let Node::Variable(obj) = *lhs {
                gen_il(*rhs);
                println!("\tstloc {}", obj.offset);
                println!("\tldc.i4.0");
            } else {
                panic!("The left-hand side of an assignment must be a variable");
            }
        }
        Node::Pop { expr } => {
            gen_il(*expr);
            println!("\tpop");
        }
        Node::Return { expr } => {
            gen_il(*expr);
            println!("\tret");
        }
        Node::UnaryOp { kind: _, expr } => {
            gen_il(*expr);
            println!("\tneg");
        }
        Node::BinaryOp { kind, lhs, rhs } => {
            gen_il(*lhs);
            gen_il(*rhs);
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
