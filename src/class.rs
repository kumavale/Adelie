use std::rc::Rc;
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
pub struct Impl<'a> {
    pub name: String,
    pub functions: Vec<Rc<Function<'a>>>,
}

impl<'a> Impl<'a> {
    pub fn new(name: String) -> Self {
        Impl {
            name,
            functions: vec![],
        }
    }
}

impl<'a> FindSymbol for [Impl<'a>] {
    type Item = Impl<'a>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}
