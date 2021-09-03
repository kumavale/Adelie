use super::ast::*;

pub fn gen_il(node: Node) {
    match node {
        Node::Integer(n) => println!("ldc.i4 {}", n as i32),
        Node::Variable { name, offset } => {
            todo!()
        }
        Node::Assign { kind, lhs, rhs } => {
            todo!()
        }
        Node::Return { expr } => {
            gen_il(*expr);
            println!("ret");
        }
        Node::UnaryOp { kind, expr } => {
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
