use std::rc::Rc;
use super::object::*;
use super::keyword::*;
use super::builtin::*;

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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CommentKind {
    LineComment,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Integer {
        typekind: Type,
        num: i32,  // -?[1-9][0-9]*
    },
    String {
        typekind: Type,
        str: String,  // ".*"
    },
    Builtin {
        kind: Builtin,
        args: Vec<Box<Node>>,
    },
    Function {
        obj: Rc<Object>,
        args: Vec<Box<Node>>,
    },
    Variable {
        typekind: Type,
        obj: Rc<Object>,
    },
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
    Comment {
        kind: CommentKind,
        comment: String,
    },
}
