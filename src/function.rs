use std::rc::Rc;
use super::ast::*;
use super::object::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Rc<Object>>,
    pub statements: Option<Node>,
    pub symbol_table: SymbolTable,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Function {
            name: name.to_string(),
            params: vec![],
            statements: None,
            symbol_table: SymbolTable::new(),
        }
    }
}
