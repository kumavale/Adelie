use super::object::*;
use super::function::*;

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

impl FindSymbol for [Struct] {
    type Item = Struct;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Impl {
    pub name: String,
    pub functions: Vec<Function>,
}

impl Impl {
    pub fn new(name: &str, functions: Vec<Function>) -> Self {
        Impl {
            name: name.to_string(),
            functions,
        }
    }
}

impl FindSymbol for [Impl] {
    type Item = Impl;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}
