use crate::ast::*;
use crate::class::Class;
use crate::keyword::{Type, RRType};
use crate::object::{FindSymbol, SymbolTable};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
    pub name: String,
    pub rettype: RRType,
    pub statements: Node<'a>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
    pub is_static: bool,
    pub is_ctor: bool,
    pub nested_class: Option<Rc<RefCell<Class<'a>>>>,
    pub local_funcs: Vec<Function<'a>>,
}

impl<'a> Function<'a> {
    pub fn new(name: &str, is_ctor: bool) -> Self {
        Function {
            name: name.to_string(),
            rettype: RRType::new(Type::Void),
            statements: new_block_node(vec![], &[]),
            symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
            is_static: true,
            is_ctor,
            nested_class: None,
            local_funcs: vec![],
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

impl<'a> FindSymbol for [Rc<Function<'a>>] {
    type Item = Rc<Function<'a>>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|f|f.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|f|f.name == name)
    }
}
