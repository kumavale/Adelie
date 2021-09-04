use super::ast::*;

pub fn gen_il(node: Node) {
    match node {
        Node::Integer(n) => println!("ldc.i4 {}", n as i32),
        Node::Variable(obj) => {
            println!("ldloc {}", obj.offset);
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
            println!("brfalse {}", else_label);
            gen_il(*then);
            println!("br {}", end_label);
            println!("{}:", else_label);
            if let Some(els) = els {
                gen_il(*els);
            }
            println!("{}:", end_label);
        }
        Node::Assign { lhs, rhs } => {
            if let Node::Variable(obj) = *lhs {
                gen_il(*rhs);
                println!("stloc {}", obj.offset);
                println!("ldc.i4.0");
            } else {
                panic!("The left-hand side of an assignment must be a variable");
            }
        }
        Node::Pop { expr } => {
            gen_il(*expr);
            println!("pop");
        }
        Node::Return { expr } => {
            gen_il(*expr);
            println!("ret");
        }
        Node::UnaryOp { kind: _, expr } => {
            gen_il(*expr);
            println!("neg");
        }
        Node::BinaryOp { kind, lhs, rhs } => {
            gen_il(*lhs);
            gen_il(*rhs);
            match kind {
                BinaryOpKind::Add => println!("add"),
                BinaryOpKind::Sub => println!("sub"),
                BinaryOpKind::Mul => println!("mul"),
                BinaryOpKind::Div => println!("div"),
                BinaryOpKind::Rem => println!("rem"),

                BinaryOpKind::Eq => println!("ceq"),
                BinaryOpKind::Lt => println!("clt"),
                BinaryOpKind::Le => {
                    println!("cgt");
                    println!("ldc.i4.0");
                    println!("ceq");
                }
                BinaryOpKind::Ne => {
                    println!("ceq");
                    println!("ldc.i4.0");
                    println!("ceq");
                }
                BinaryOpKind::Gt => println!("cgt"),
                BinaryOpKind::Ge => {
                    println!("clt");
                    println!("ldc.i4.0");
                    println!("ceq");
                }
            }
        }
    }
}
