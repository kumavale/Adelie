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
    Shl,     // <<
    Shr,     // >>
    Eq,      // ==
    Lt,      // <
    Le,      // <=
    Ne,      // !=
    Gt,      // >
    Ge,      // >=
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ShortCircuitOpKind {
    And,  // &&
    Or,   // ||
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
    Struct {
        obj: Rc<Object>,
        field: Vec<Node>,
    },
    Field {
        name: String,
        expr: Box<Node>,
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
    Loop {
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
    Cast {
        typekind: Type,
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
    ShortCircuitOp {
        kind: ShortCircuitOpKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    }
}
