use std::fmt;
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
pub struct Node {
    pub kind: NodeKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind {
    Integer {
        ty: Type,
        num: i128,  // -?[1-9][0-9]*
    },
    String {
        ty: Type,
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
    Method {
        expr: Box<Node>,
        ident: String,
        args: Vec<Node>,
    },
    Struct {
        obj: Rc<Object>,
        field: Vec<Node>,
    },
    Field {
        expr: Box<Node>,
        ident: String,
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
        brk_label_seq: usize,
    },
    Loop {
        then: Box<Node>,
        brk_label_seq: usize,
    },
    Assign {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Return {
        expr: Option<Box<Node>>,
    },
    Break {
        brk_label_seq: usize,
    },
    Cast {
        ty: Type,
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
    },
    Semi {
        expr: Box<Node>,
    },
    Path {
        segment: String,
        child: Box<Node>,
    },
    Empty,
}

pub fn new_binary_op_node(kind: BinaryOpKind, lhs: Node, rhs: Node) -> Node {
    Node {
        kind: NodeKind::BinaryOp {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    }
}

pub fn new_unary_op_node(kind: UnaryOpKind, expr: Node) -> Node {
    Node {
        kind: NodeKind::UnaryOp {
            kind,
            expr: Box::new(expr),
        },
    }
}

pub fn new_assign_node(lhs: Node, rhs: Node) -> Node {
    Node {
        kind: NodeKind::Assign {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    }
}

pub fn new_short_circuit_op_node(kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Node {
    Node {
        kind: NodeKind::ShortCircuitOp {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    }
}

pub fn new_if_node(cond: Node, then: Node, els: Option<Node>) -> Node {
    Node {
        kind: NodeKind::If {
            cond: Box::new(cond),
            then: Box::new(then),
            els: els.map(Box::new),
        },
    }
}

pub fn new_while_node(cond: Node, then: Node, brk_label_seq: usize) -> Node {
    Node {
        kind: NodeKind::While {
            cond: Box::new(cond),
            then: Box::new(then),
            brk_label_seq,
        },
    }
}

pub fn new_loop_node(then: Node, brk_label_seq: usize) -> Node {
    Node {
        kind: NodeKind::Loop {
            then: Box::new(then),
            brk_label_seq,
        },
    }
}

pub fn new_block_node(stmts: Vec<Node>) -> Node {
    Node {
        kind: NodeKind::Block {
            stmts,
        },
    }
}

pub fn new_return_node(expr: Option<Node>) -> Node {
    Node {
        kind: NodeKind::Return {
            expr: expr.map(Box::new),
        },
    }
}

pub fn new_break_node(brk_label_seq: usize) -> Node {
    Node {
        kind: NodeKind::Break {
            brk_label_seq,
        },
    }
}

pub fn new_num_node(num: i128) -> Node {
    Node {
        kind: NodeKind::Integer {
            ty: Type::Numeric(Numeric::Integer),
            num,
        },
    }
}

pub fn new_char_node(c: char) -> Node {
    Node {
        kind: NodeKind::Integer {
            ty: Type::Char,
            num: c as i128
        },
    }
}

pub fn new_string_node(s: &str) -> Node {
    Node {
        kind: NodeKind::String {
            ty: Type::String,
            str: s.to_string(),
        },
    }
}

pub fn new_bool_node(b: Keyword) -> Node {
    Node {
        kind: NodeKind::Integer {
            ty: Type::Bool,
            num: match b {
                Keyword::True  => 1,
                Keyword::False => 0,
                _ => unreachable!(),
            },
        },
    }
}

pub fn new_cast_node(ty: Type, expr: Node) -> Node {
    Node {
        kind: NodeKind::Cast {
            ty,
            expr: Box::new(expr),
        },
    }
}

pub fn new_builtin_call_node(kind: Builtin, args: Vec<Node>) -> Node {
    Node {
        kind: NodeKind::Builtin {
            kind,
            args,
        },
    }
}

pub fn new_function_call_node(name: &str, args: Vec<Node>) -> Node {
    Node {
        kind: NodeKind::Call {
            name: name.to_string(),
            args,
        },
    }
}

pub fn new_struct_expr_node(symbol_table: &mut SymbolTable, name: &str, field: Vec<Node>) -> Node {
    fn seq() -> usize {
        unsafe {
            static mut ID: usize = 0;
            ID += 1;
            ID
        }
    }
    let unique_name = format!("{}:{}", name, seq());
    let obj = Rc::new(Object::new(unique_name, symbol_table.len(), false, Type::Struct(name.to_string())));
    symbol_table.push(Rc::clone(&obj));
    Node {
        kind: NodeKind::Struct {
            obj,
            field,
        },
    }
}

pub fn new_field_node(expr: Node, ident: String) -> Node {
    Node {
        kind: NodeKind::Field {
            expr: Box::new(expr),
            ident,
        },
    }
}

pub fn new_method_call_node(expr: Node, ident: String, args: Vec<Node>) -> Node {
    Node {
        kind: NodeKind::Method {
            expr: Box::new(expr),
            ident,
            args,
        },
    }
}

pub fn new_variable_node(obj: &Rc<Object>) -> Node {
    Node {
        kind: NodeKind::Variable {
            obj: Rc::clone(obj),
        },
    }
}

pub fn new_variable_node_with_let(symbol_table: &mut SymbolTable, ident: String, ty: Type) -> Node {
    let obj = Rc::new(Object::new(ident, symbol_table.len(), false, ty));
    symbol_table.push(Rc::clone(&obj));
    Node {
        kind: NodeKind::Variable {
            obj,
        },
    }
}

pub fn new_empty_node() -> Node {
    Node {
        kind: NodeKind::Empty,
    }
}

pub fn new_semi_node(expr: Node) -> Node {
    Node {
        kind: NodeKind::Semi {
            expr: Box::new(expr),
        },
    }
}

pub fn new_path_node(segment: &str, child: Node) -> Node {
    Node {
        kind: NodeKind::Path {
            segment: segment.to_string(),
            child: Box::new(child),
        },
    }
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOpKind::Neg   => write!(f, "-"),
            UnaryOpKind::Not   => write!(f, "!"),
            UnaryOpKind::Ref   => write!(f, "&"),
            UnaryOpKind::Deref => write!(f, "*"),
        }
    }
}

impl fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOpKind::Add    => write!(f, "+"),
            BinaryOpKind::Sub    => write!(f, "-"),
            BinaryOpKind::Mul    => write!(f, "*"),
            BinaryOpKind::Div    => write!(f, "/"),
            BinaryOpKind::Rem    => write!(f, "%"),
            BinaryOpKind::BitXor => write!(f, "^"),
            BinaryOpKind::BitAnd => write!(f, "&"),
            BinaryOpKind::BitOr  => write!(f, "|"),
            BinaryOpKind::Shl    => write!(f, "<<"),
            BinaryOpKind::Shr    => write!(f, ">>"),
            BinaryOpKind::Eq     => write!(f, "=="),
            BinaryOpKind::Lt     => write!(f, "<"),
            BinaryOpKind::Le     => write!(f, "<="),
            BinaryOpKind::Ne     => write!(f, "!="),
            BinaryOpKind::Gt     => write!(f, ">"),
            BinaryOpKind::Ge     => write!(f, ">="),
        }
    }
}

impl fmt::Display for ShortCircuitOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ShortCircuitOpKind::And => write!(f, "&&"),
            ShortCircuitOpKind::Or  => write!(f, "||"),
        }
    }
}
