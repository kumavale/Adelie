use std::rc::Rc;
use super::function::*;

#[derive(Clone, Debug, PartialEq)]
pub struct NameSpace {
    pub name: String,
    pub children: Vec<NameSpace>,
    pub elements: Vec<Rc<Function>>,
}

impl NameSpace {
    pub fn new(name: &str) -> Self {
        NameSpace {
            name: name.to_string(),
            children: vec![],
            elements: vec![],
        }
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut Self> {
        self.children
            .iter_mut()
            .find(|n| n.name == name)
    }

    pub fn find_mut_recursive(&mut self, name: &str) -> Option<&mut Self> {
        if self.name == name {
            Some(self)
        } else {
            self.children
                .iter_mut()
                .find_map(|n| n.find_mut_recursive(name))
        }
    }
}
