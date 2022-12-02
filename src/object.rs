use std::cell::RefCell;
use std::rc::Rc;
use super::keyword::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub name: String,
    pub offset: usize,
    pub is_param: bool,
    pub ty: Type,
    pub assigned: bool,
    pub mutable: bool,
}

impl Object {
    pub fn new(name: String, offset: usize, is_param: bool, ty: Type, mutable: bool) -> Self {
        Object {
            name,
            offset,
            is_param,
            ty,
            assigned: false,
            mutable,
        }
    }

    pub fn is_assigned(&self) -> bool {
        self.assigned
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn is_param(&self) -> bool {
        self.is_param
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub objs: Vec<Rc<RefCell<Object>>>,
    pub scopes: Vec<Vec<Rc<RefCell<Object>>>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            objs: vec![],
            scopes: vec![vec![]],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(vec![]);
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn push(&mut self, obj: Rc<RefCell<Object>>) {
        self.objs.push(Rc::clone(&obj));
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.push(obj);
        }
    }

    pub fn len(&self) -> usize {
        self.objs.len()
    }
}

pub trait FindSymbol {
    type Item;
    fn find(&self, name: &str) -> Option<&Self::Item>;
    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item>;
}

impl FindSymbol for SymbolTable {
    type Item = Rc<RefCell<Object>>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        for scope in self.scopes.iter().rev() {
            if let Some(obj) = scope.iter().rev().find(|o|o.borrow().name == name) {
                return Some(obj)
            }
        }
        None
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(obj) = scope.iter_mut().rev().find(|o|o.borrow().name == name) {
                return Some(obj)
            }
        }
        None
    }
}
