use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use super::builtin::*;
use super::keyword::*;
use super::object::*;
use super::token::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
    Neg,    // -
    Not,    // !
    Ref,    // &
    Deref,  // *
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ShortCircuitOpKind {
    And,  // &&
    Or,   // ||
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node<'a> {
    pub kind: NodeKind<'a>,
    pub token: &'a [Token],
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind<'a> {
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
        args: Vec<Node<'a>>,
    },
    Call {
        name: String,
        args: Vec<Node<'a>>,
    },
    Method {
        expr: Box<Node<'a>>,
        ident: String,
        args: Vec<Node<'a>>,
    },
    Struct {
        obj: Rc<RefCell<Object>>,
        field: Vec<Node<'a>>,
    },
    Field {
        expr: Box<Node<'a>>,
        ident: String,
    },
    Variable {
        obj: Rc<RefCell<Object>>,
    },
    Block {
        stmts: Vec<Node<'a>>,
    },
    If {
        cond: Box<Node<'a>>,
        then: Box<Node<'a>>,
        els: Option<Box<Node<'a>>>,
    },
    While {
        cond: Box<Node<'a>>,
        then: Box<Node<'a>>,
        brk_label_seq: usize,
    },
    Loop {
        then: Box<Node<'a>>,
        brk_label_seq: usize,
    },
    Assign {
        lhs: Box<Node<'a>>,
        rhs: Box<Node<'a>>,
    },
    Return {
        expr: Option<Box<Node<'a>>>,
    },
    Break {
        brk_label_seq: usize,
    },
    Cast {
        ty: Type,
        expr: Box<Node<'a>>,
    },
    UnaryOp {
        kind: UnaryOpKind,
        expr: Box<Node<'a>>,
    },
    BinaryOp {
        kind: BinaryOpKind,
        lhs: Box<Node<'a>>,
        rhs: Box<Node<'a>>,
    },
    ShortCircuitOp {
        kind: ShortCircuitOpKind,
        lhs: Box<Node<'a>>,
        rhs: Box<Node<'a>>,
    },
    Semi {
        expr: Box<Node<'a>>,
    },
    Path {
        segment: String,
        child: Box<Node<'a>>,
    },
    Empty,
}

pub fn new_binary_op_node<'a>(
    kind: BinaryOpKind,
    lhs: Node<'a>,
    rhs: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::BinaryOp {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
        token,
    }
}

pub fn new_unary_op_node<'a>(
    kind: UnaryOpKind,
    expr: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::UnaryOp {
            kind,
            expr: Box::new(expr),
        },
        token,
    }
}

pub fn new_assign_node<'a>(
    lhs: Node<'a>,
    rhs: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Assign {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
        token,
    }
}

pub fn new_short_circuit_op_node<'a>(
    kind: ShortCircuitOpKind,
    lhs: Node<'a>,
    rhs: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::ShortCircuitOp {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
        token,
    }
}

pub fn new_if_node<'a>(
    cond: Node<'a>,
    then: Node<'a>,
    els: Option<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::If {
            cond: Box::new(cond),
            then: Box::new(then),
            els: els.map(Box::new),
        },
        token,
    }
}

pub fn new_while_node<'a>(
    cond: Node<'a>,
    then: Node<'a>,
    brk_label_seq: usize,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::While {
            cond: Box::new(cond),
            then: Box::new(then),
            brk_label_seq,
        },
        token,
    }
}

pub fn new_loop_node<'a>(
    then: Node<'a>,
    brk_label_seq: usize,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Loop {
            then: Box::new(then),
            brk_label_seq,
        },
        token,
    }
}

pub fn new_block_node<'a>(
    stmts: Vec<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Block {
            stmts,
        },
        token,
    }
}

pub fn new_return_node<'a>(
    expr: Option<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Return {
            expr: expr.map(Box::new),
        },
        token,
    }
}

pub fn new_break_node(
    brk_label_seq: usize,
    token: &[Token],
) -> Node<'_> {
    Node {
        kind: NodeKind::Break {
            brk_label_seq,
        },
        token,
    }
}

pub fn new_num_node(
    num: i128,
    token: &[Token],
) -> Node<'_> {
    Node {
        kind: NodeKind::Integer {
            ty: Type::Numeric(Numeric::Integer),
            num,
        },
        token,
    }
}

pub fn new_char_node(
    c: char,
    token: &[Token],
) -> Node<'_> {
    Node {
        kind: NodeKind::Integer {
            ty: Type::Char,
            num: c as i128
        },
        token,
    }
}

pub fn new_string_node<'a>(
    s: &str,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::String {
            ty: Type::String,
            str: s.to_string(),
        },
        token,
    }
}

pub fn new_bool_node(
    b: Keyword,
    token: &[Token],
) -> Node<'_> {
    Node {
        kind: NodeKind::Integer {
            ty: Type::Bool,
            num: match b {
                Keyword::True  => 1,
                Keyword::False => 0,
                _ => unreachable!(),
            },
        },
        token,
    }
}

pub fn new_cast_node<'a>(
    ty: Type,
    expr: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Cast {
            ty,
            expr: Box::new(expr),
        },
        token,
    }
}

pub fn new_builtin_call_node<'a>(
    kind: Builtin,
    args: Vec<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Builtin {
            kind,
            args,
        },
        token,
    }
}

pub fn new_function_call_node<'a>(
    name: &str,
    args: Vec<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Call {
            name: name.to_string(),
            args,
        },
        token,
    }
}

pub fn new_struct_expr_node<'a>(
    symbol_table: &mut SymbolTable,
    name: &str,
    field: Vec<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    fn seq() -> usize {
        unsafe {
            static mut ID: usize = 0;
            ID += 1;
            ID
        }
    }
    let unique_name = format!("{}:{}", name, seq());
    let obj = Rc::new(RefCell::new(Object::new(unique_name, symbol_table.len(), false, Type::Struct(name.to_string()), false)));
    obj.borrow_mut().assigned = true;
    symbol_table.push(Rc::clone(&obj));
    Node {
        kind: NodeKind::Struct {
            obj,
            field,
        },
        token,
    }
}

pub fn new_field_node<'a>(
    expr: Node<'a>,
    ident: String,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Field {
            expr: Box::new(expr),
            ident,
        },
        token,
    }
}

pub fn new_method_call_node<'a>(
    expr: Node<'a>,
    ident: String,
    args: Vec<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Method {
            expr: Box::new(expr),
            ident,
            args,
        },
        token,
    }
}

pub fn new_variable_node<'a>(
    obj: &Rc<RefCell<Object>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Variable {
            obj: Rc::clone(obj),
        },
        token,
    }
}

pub fn new_variable_node_with_let<'a>(
    symbol_table: &mut SymbolTable,
    ident: String,
    ty: Type,
    token: &'a [Token],
    mutable: bool,
) -> Node<'a> {
    let obj = Rc::new(RefCell::new(Object::new(ident, symbol_table.len(), false, ty, mutable)));
    symbol_table.push(Rc::clone(&obj));
    Node {
        kind: NodeKind::Variable {
            obj,
        },
        token,
    }
}

pub fn new_empty_node<'a>() -> Node<'a> {
    Node {
        kind: NodeKind::Empty,
        token: &[],
    }
}

pub fn new_semi_node<'a>(
    expr: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Semi {
            expr: Box::new(expr),
        },
        token,
    }
}

pub fn new_path_node<'a>(
    segment: &str,
    child: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Path {
            segment: segment.to_string(),
            child: Box::new(child),
        },
        token,
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
