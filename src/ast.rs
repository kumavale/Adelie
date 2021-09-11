use std::rc::Rc;
use super::object::*;
use super::keyword::*;
use super::builtin::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOpKind {
    Neg,    // -
    Not,    // !
    Ref,    // &
    Deref,  // *
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOpKind {
    Add,     // +
    Sub,     // -
    Mul,     // *
    Div,     // /
    Rem,     // %
    BitXor,  // ^
    BitAnd,  // &
    BitOr,   // |
    Eq,      // ==
    Lt,      // <
    Le,      // <=
    Ne,      // !=
    Gt,      // >
    Ge,      // >=
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
        args: Vec<Node>,
    },
    Call {
        name: String,
        args: Vec<Node>,
    },
    Variable {
        obj: Rc<Object>,
    },
    Block {
        stmts: Vec<Node>,
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
    Evaluates {
        expr: Box<Node>,
    },
    Return {
        expr: Option<Box<Node>>,
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
