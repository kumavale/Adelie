use super::object::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: String,
    pub field: Vec<Object>,
}

impl Struct {
    pub fn new() -> Self {
        Struct {
            name: String::new(),
            field: vec![],
        }
    }
}

pub trait StructSymbolTable {
    fn find_struct(&self, name: &str) -> Option<&Struct>;
}

impl StructSymbolTable for [Struct] {
    fn find_struct(&self, name: &str) -> Option<&Struct> {
        self.iter().find(|s|s.name == name)
    }
}
