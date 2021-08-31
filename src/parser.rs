use super::ast::*;

pub fn eval(ast: &Option<Box<Node>>) -> u64 {
    match &**ast.as_ref().unwrap() {
        Node::Integer(n) => *n,
        Node::BinaryOp { kind, lhs, rhs } => {
            match kind {
                BinaryOpKind::Add => eval(lhs) + eval(rhs),
                BinaryOpKind::Sub => eval(lhs) - eval(rhs),
                BinaryOpKind::Mul => eval(lhs) * eval(rhs),
                BinaryOpKind::Div => eval(lhs) / eval(rhs),
                BinaryOpKind::Mod => eval(lhs) % eval(rhs),
            }
        }
    }
}
