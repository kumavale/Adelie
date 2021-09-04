use std::rc::Rc;
use super::object::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOpKind {
    Neg,  // -
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOpKind {
    Add,  // +
    Sub,  // -
    Mul,  // *
    Div,  // /
    Rem,  // %
    Eq,   // ==
    Lt,   // <
    Le,   // <=
    Ne,   // !=
    Gt,   // >
    Ge,   // >=
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Integer(i32),  // -?[1-9][0-9]*
    Function(Rc<Object>),
    Variable(Rc<Object>),
    Block {
        stmts: Vec<Box<Node>>,
    },
    If {
        cond: Box<Node>,
        then: Box<Node>,
        els: Option<Box<Node>>,
    },
    While {
        cond: Box<Node>,
        then: Box<Node>,
    },
    Assign {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Pop {
        expr: Box<Node>,
    },
    Return {
        expr: Box<Node>,
    },
    UnaryOp {
        kind: UnaryOpKind,
        expr: Box<Node>,
    },
    BinaryOp {
        kind: BinaryOpKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}
