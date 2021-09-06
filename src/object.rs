use std::rc::Rc;
use super::keyword::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub name: String,
    pub offset: usize,
    pub is_param: bool,
    pub typekind: Type,
}

impl Object {
    pub fn new(name: String, offset: usize, is_param: bool, typekind: Type) -> Self {
        Object {
            name,
            offset,
            is_param,
            typekind,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub objs: Vec<Rc<Object>>,
    pub scopes: Vec<Vec<Rc<Object>>>,
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

    pub fn push(&mut self, obj: Rc<Object>) {
        self.objs.push(Rc::clone(&obj));
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.push(obj);
        }
    }

    pub fn find_name_current_scope(&self, name: &str) -> Option<&Rc<Object>> {
        if let Some(current_scope) = self.scopes.last() {
            for obj in current_scope {
                if obj.name == name {
                    return Some(obj);
                }
            }
        }
        None
    }

    pub fn find_name(&self, name: &str) -> Option<&Rc<Object>> {
        for scope in self.scopes.iter().rev() {
            for obj in scope {
                if obj.name == name {
                    return Some(obj);
                }
            }
        }
        None
    }

    pub fn len(&self) -> usize {
        self.objs.len()
    }
}
