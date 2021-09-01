use super::ast::*;

pub fn eval(node: &Option<Box<Node>>) -> u64 {
    match &**node.as_ref().unwrap() {
        Node::Integer(n) => *n,
        Node::BinaryOp { kind, lhs, rhs } => {
            match kind {
                BinaryOpKind::Add => eval(lhs) + eval(rhs),
                BinaryOpKind::Sub => eval(lhs) - eval(rhs),
                BinaryOpKind::Mul => eval(lhs) * eval(rhs),
                BinaryOpKind::Div => eval(lhs) / eval(rhs),
                BinaryOpKind::Rem => eval(lhs) % eval(rhs),
            }
        }
    }
}

pub fn gen_il(node: &Option<Box<Node>>) {
    match &**node.as_ref().unwrap() {
        Node::Integer(n) => println!("ldc.i4 {}", *n as i32),
        Node::BinaryOp { kind, lhs, rhs } => {
            gen_il(lhs);
            gen_il(rhs);
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
