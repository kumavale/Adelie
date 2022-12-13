use crate::ast::*;
use crate::keyword::{Type, RRType};
use crate::object::{FindSymbol, SymbolTable};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function<'a> {
    pub name: String,
    pub rettype: RRType,
    pub statements: Node<'a>,
    pub lvar_symbol_table: Rc<RefCell<SymbolTable>>,
    pub param_symbol_table: SymbolTable,
    pub is_static: bool,
    pub is_ctor: bool,
}

impl<'a> Function<'a> {
    pub fn new(name: &str, is_ctor: bool) -> Self {
        Function {
            name: name.to_string(),
            rettype: RRType::new(Type::Void),
            statements: new_block_node(vec![], &[]),
            lvar_symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
            param_symbol_table: SymbolTable::new(),
            is_static: true,
            is_ctor,
        }
    }
}

impl<'a> FindSymbol for [Function<'a>] {
    type Item = Function<'a>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|f|f.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|f|f.name == name)
    }
}
