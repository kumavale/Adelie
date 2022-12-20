use crate::builtin::*;
use crate::class::{Class, ClassKind, Impl, EnumDef};
use crate::function::Function;
use crate::keyword::{Keyword, Numeric, Type, RRType};
use crate::object::{Object, ObjectKind, SymbolTable};
use crate::token::Token;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Node<'a> {
    pub kind: NodeKind<'a>,
    pub token: &'a [Token],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NodeKind<'a> {
    Integer {
        ty: Type,
        num: i128,  // -?[1-9][0-9]*
    },
    String {
        ty: Type,
        str: String,  // ".*"
    },
    Box {
        method: Box<Node<'a>>,
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
    Lambda {
        ty: Type,
        ident: String,
        // args: Vec<Node<'a>>,
    },
    Struct {
        obj: Rc<RefCell<Object>>,
        field: Vec<Node<'a>>,
    },
    FieldOrProperty {
        lvar_symbol_table: Rc<RefCell<SymbolTable>>,
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
        ty: RRType,
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
    ty: RRType,
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
    reference: Option<String>,
    name: &str,
    field: Vec<Node<'a>>,
    token: &'a [Token],
    current_mod: Vec<String>,
) -> Node<'a> {
    let unique_name = format!("{}:{}", name, crate::seq!());
    let obj = Rc::new(RefCell::new(
            Object::new(unique_name,
                        symbol_table.len(),
                        ObjectKind::Local,
                        RRType::new(Type::Class(ClassKind::Struct, reference, current_mod, name.to_string(), None, false)),
                        false)));
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

pub fn new_field_or_property_node<'a>(
    lvar_symbol_table: Rc<RefCell<SymbolTable>>,
    expr: Node<'a>,
    ident: String,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::FieldOrProperty {
            lvar_symbol_table,
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

pub fn new_lambda_node<'a>(
    ty: Type,
    ident: String,
    // args: Vec<Node<'a>>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Lambda {
            ty,
            ident,
            // args,
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
    ty: RRType,
    token: &'a [Token],
    mutable: bool,
    assigned: bool,
    kind: ObjectKind,
) -> Node<'a> {
    let obj = Rc::new(RefCell::new(Object::new(ident, symbol_table.len(), kind, ty, mutable)));
    obj.borrow_mut().assigned = assigned;
    symbol_table.push(Rc::clone(&obj));
    Node {
        kind: NodeKind::Variable {
            obj,
        },
        token,
    }
}

pub fn new_box_node<'a>(
    method: Node<'a>,
    token: &'a [Token],
) -> Node<'a> {
    Node {
        kind: NodeKind::Box {
            method: Box::new(method),
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

#[derive(Clone, Debug)]
pub struct Item<'a> {
    pub attrs: Vec<Attribute>,
    pub kind: ItemKind<'a>,
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub item: AttrItem,
}

impl Attribute {
    pub fn find_item(&self, name: &str) -> Option<&AttrItem> {
        match &self.item {
            AttrItem::Delimited(n, _) /*| AttrItem::Eq(n, _)*/ => {
                if n == name { Some(&self.item) } else { None }
            }
        }
    }

    pub fn find_value(&self, key: &str) -> Option<&str> {
        match &self.item {
            AttrItem::Delimited(_, kvs) /*| AttrItem::Eq(k, v)*/ => {
                for (k, v) in kvs {
                    if k == key { return Some(v) }
                }
                None
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum AttrItem {
    ///// No arguments: `#[attr]`.
    //Value(String),
    /// Delimited arguments: `#[attr(key = "value")]`.
    Delimited(String, Vec<(String, String)>),
    ///// Arguments of a key-value attribute: `#[attr = "value"]`.
    //Eq(String, String),
}

#[derive(Clone, Debug)]
pub enum ItemKind<'a> {
    /// A function declaration (`fn`).
    ///
    /// E.g., `fn foo(bar: usize) -> usize { .. }`.
    Fn(Function<'a>),
    /// A module declaration (`mod`).
    ///
    /// E.g., `mod foo;` or `mod foo { .. }`.
    Mod((String, Vec<(usize, Item<'a>)>)),  // (ident, items)
    /// An external module (`extern`).
    ///
    /// E.g., `extern {}`.
    ForeignMod(Vec<ForeignItemKind<'a>>),
    /// An enum definition (`enum`).
    ///
    /// E.g., `enum Foo<A, B> { C<A>, D<B> }`.
    Enum(EnumDef),
    /// A struct definition (`struct`).
    ///
    /// E.g., `struct Foo<A> { x: A }`.
    Class(Class<'a>),
    /// An implementation.
    ///
    /// E.g., `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`.
    Impl(Impl<'a>),
}

#[derive(Clone, Debug)]
pub enum ForeignItemKind<'a> {
    Fn(Function<'a>),
    Mod((String, Vec<(usize, Item<'a>)>)),
    Enum(EnumDef),
    Class(Class<'a>),
    Impl(Impl<'a>),
}
