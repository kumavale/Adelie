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

impl FindSymbol for [Struct] {
    type Item = Struct;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}
