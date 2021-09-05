use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub name: String,
    pub offset: usize,
    pub is_param: bool,
}

impl Object {
    pub fn new(name: String, offset: usize, is_param: bool) -> Self {
        Object {
            name,
            offset,
            is_param,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub objs: Vec<Rc<Object>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { objs: vec![] }
    }

    pub fn push(&mut self, obj: Rc<Object>) {
        self.objs.push(obj);
    }

    pub fn find_lvar(&self, name: &str) -> Option<&Rc<Object>> {
        for obj in &self.objs {
            if obj.name == name {
                return Some(obj);
            }
        }
        None
    }

    pub fn len(&self) -> usize {
        self.objs.len()
    }
}
