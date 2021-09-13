use std::rc::Rc;
use super::object::*;
use super::keyword::*;
use super::function::*;
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

pub fn new_binary_op_node(kind: BinaryOpKind, lhs: Node, rhs: Node) -> Node {
    Node::BinaryOp {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn new_unary_op_node(kind: UnaryOpKind, expr: Node) -> Node {
    Node::UnaryOp {
        kind,
        expr: Box::new(expr),
    }
}

pub fn new_assign_node(lhs: Node, rhs: Node) -> Node {
    Node::Assign {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn new_short_circuit_op_node(kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Node {
    Node::ShortCircuitOp {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn new_if_node(cond: Node, then: Node, els: Option<Node>) -> Node {
    Node::If {
        cond: Box::new(cond),
        then: Box::new(then),
        els: els.map(Box::new),
    }
}

pub fn new_while_node(cond: Node, then: Node) -> Node {
    Node::While {
        cond: Box::new(cond),
        then: Box::new(then),
    }
}

pub fn new_loop_node(then: Node) -> Node {
    Node::Loop {
        then: Box::new(then),
    }
}

pub fn new_block_node(stmts: Vec<Node>) -> Node {
    Node::Block {
        stmts,
    }
}

pub fn new_return_node(expr: Option<Node>) -> Node {
    Node::Return {
        expr: expr.map(Box::new),
    }
}

pub fn new_evaluates_node(expr: Node) -> Node {
    Node::Evaluates {
        expr: Box::new(expr),
    }
}

pub fn new_num_node(num: i32) -> Node {
    Node::Integer {
        typekind: Type::Numeric(Numeric::I32),
        num,
    }
}

pub fn new_char_node(c: char) -> Node {
    Node::Integer {
        typekind: Type::Char,
        num: c as i32
    }
}

pub fn new_string_node(s: &str) -> Node {
    Node::String {
        typekind: Type::String,
        str: s.to_string(),
    }
}

pub fn new_bool_node(b: Keyword) -> Node {
    Node::Integer {
        typekind: Type::Bool,
        num: match b {
            Keyword::True  => 1,
            Keyword::False => 0,
            _ => unreachable!(),
        },
    }
}

pub fn new_cast_node(typekind: Type, expr: Node) -> Node {
    Node::Cast {
        typekind,
        expr: Box::new(expr),
    }
}

pub fn new_builtin_call_node(kind: Builtin, args: Vec<Node>) -> Node {
    Node::Builtin {
        kind,
        args,
    }
}

pub fn new_function_call_node(name: &str, args: Vec<Node>) -> Node {
    Node::Call {
        name: name.to_string(),
        args,
    }
}

pub fn new_struct_expr_node(symbol_table: &mut SymbolTable, name: &str, field: Vec<Node>) -> Node {
    fn seq() -> usize {
        unsafe {
            static mut SEQ: usize = 0;
            SEQ += 1;
            SEQ
        }
    }
    let unique_name = format!("{}{}", name, seq());
    let obj = Rc::new(Object::new(unique_name, symbol_table.len(), false, Type::Struct(name.to_string())));
    symbol_table.push(Rc::clone(&obj));
    Node::Struct {
        obj,
        field,
    }
}

pub fn new_field_node(function: &mut Function, name: &str, expr: Node) -> Node {
    Node::Field {
        name: name.to_string(),
        expr: Box::new(expr),
    }
}

pub fn new_variable_node(function: &mut Function, name: &str) -> Node {
    if let Some(obj) = function.lvar_symbol_table.find(name) {
        Node::Variable {
            obj: Rc::clone(obj),
        }
    } else if let Some(obj) = function.param_symbol_table.find(name) {
        Node::Variable {
            obj: Rc::clone(obj),
        }
    } else {
        panic!("The name '{}' does not exist in the current context", name)
    }
}

pub fn new_variable_node_with_let(symbol_table: &mut SymbolTable, name: &str, typekind: Type) -> Node {
    if symbol_table.find_name_current_scope(name).is_some() {
        panic!("A local variable or function named '{}' is already defined in this scope", name)
    } else {
        let obj = Rc::new(Object::new(name.to_string(), symbol_table.len(), false, typekind));
        symbol_table.push(Rc::clone(&obj));
        Node::Variable {
            obj,
        }
    }
}
