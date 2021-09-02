use super::ast::*;

pub fn eval(node: Node) -> i64 {
    match node {
        Node::Integer(n) => n as i64,
        Node::UnaryOp { kind, expr } => {
            -eval(*expr)
        }
        Node::BinaryOp { kind, lhs, rhs } => {
            match kind {
                BinaryOpKind::Add => eval(*lhs) + eval(*rhs),
                BinaryOpKind::Sub => eval(*lhs) - eval(*rhs),
                BinaryOpKind::Mul => eval(*lhs) * eval(*rhs),
                BinaryOpKind::Div => eval(*lhs) / eval(*rhs),
                BinaryOpKind::Rem => eval(*lhs) % eval(*rhs),
            }
        }
    }
}

pub fn gen_il(node: Node) {
    match node {
        Node::Integer(n) => println!("ldc.i4 {}", n as i32),
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
            }
        }
    }
}
