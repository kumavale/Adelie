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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssignOpKind {
    Assign,  // =
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Integer(i32),  // -?[1-9][0-9]*
    Variable(Object),
    Assign {
        kind: AssignOpKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
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

impl Node {
    pub fn offset(&self) -> Option<usize> {
        if let Node::Variable(obj) = self {
            Some(obj.offset)
        } else {
            None
        }
    }
}
