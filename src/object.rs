use crate::keyword::RRType;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ObjectKind {
    Field,
    Local,
    Param,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub name: String,
    pub kind: ObjectKind,
    pub offset: usize,
    pub ty: RRType,
    pub assigned: bool,
    pub mutable: bool,
    pub used: bool,
    pub parent: Option<Rc<RefCell<Object>>>,
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
            used: false,
            parent: None,
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

    pub fn consume(&mut self) {
        self.used = true;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumObject {
    pub name: String,
    pub value: usize,
}

impl EnumObject {
    pub fn new(name: String, value: usize) -> Self {
        EnumObject {
            name,
            value,
        }
    }
}

type Vars = Vec<Rc<RefCell<Object>>>;

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub objs: Vars,
    pub scopes: Vec<Vars>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            objs: vec![],
            scopes: vec![vec![]],
            //           ^^^^^^ 仮引数用
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

    pub fn offset(&self, kind: ObjectKind) -> usize {
        self.objs.iter().fold(0, |acc, x| if x.borrow().kind == kind { acc+1 } else { acc })
    }

    pub fn drain(&mut self, name: &str) -> Option<Rc<RefCell<Object>>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((i, _)) = scope.iter().enumerate().find(|(_,o)|o.borrow().name == name) {
                let obj = scope.remove(i);
                let (i, _) = self.objs.iter().enumerate().find(|(_,o)| **o == obj).unwrap();
                let obj = self.objs.remove(i);
                self.repair_offset();
                return Some(obj);
            }
        }
        None
    }

    pub fn clear_local(&mut self) {
        self.objs = self.objs.iter().filter(|o|o.borrow().kind != ObjectKind::Local).cloned().collect();
    }

    pub fn repair_offset(&mut self) {
        // とりあえずObjectKind::Localだけ修正
        self.objs.iter()
            .filter(|o|o.borrow().kind == ObjectKind::Local)
            .fold(0, |acc, o| {
                o.borrow_mut().offset = acc;
                acc + 1
            });
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
