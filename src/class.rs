use super::object::*;
use super::function::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: String,
    pub field: Vec<Object>,
    pub functions: Vec<Function>,
}

impl Struct {
    pub fn new() -> Self {
        Struct {
            name: String::new(),
            field: vec![],
            functions: vec![],
        }
    }
}

pub trait StructSymbolTable {
    fn find_struct(&self, name: &str) -> Option<&Struct>;
    fn find_struct_mut(&mut self, name: &str) -> Option<&mut Struct>;
}

impl StructSymbolTable for [Struct] {
    fn find_struct(&self, name: &str) -> Option<&Struct> {
        self.iter().find(|s|s.name == name)
    }

    fn find_struct_mut(&mut self, name: &str) -> Option<&mut Struct> {
        self.iter_mut().find(|s|s.name == name)
    }
}
