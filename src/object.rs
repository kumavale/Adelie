use crate::keyword::RRType;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ObjectKind {
    Field,
    Local,
    Param,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Object {
    pub name: String,
    pub kind: ObjectKind,
    pub offset: usize,  // TODO: シンボルテーブルを元に計算すれば良い
    pub ty: RRType,
    pub assigned: bool,
    pub mutable: bool,
}

impl Object {
    pub fn new(name: String, offset: usize, kind: ObjectKind, ty: RRType, mutable: bool) -> Self {
        Object {
            name,
            kind,
            offset,
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
        self.kind == ObjectKind::Param
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

    pub fn drain(&mut self, name: &str) -> Option<Rc<RefCell<Object>>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((i, _)) = scope.iter().enumerate().find(|(_,o)|o.borrow().name == name) {
                return Some(scope.swap_remove(i));
            }
        }
        None
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
            if let Some(obj) = scope.iter_mut().find(|o|o.borrow().name == name) {
                return Some(obj)
            }
        }
        None
    }
}
