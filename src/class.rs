use crate::function::Function;
use crate::object::{Object, FindSymbol};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Dummy();

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Struct<'a> {
    pub name: String,
    pub field: Vec<Object>,
    pub properties: Vec<Object>,
    pub path: Vec<String>,
    pub reference: Option<String>,
    //pub impls: Vec<Impl<'a>>,  // trait毎
    pub _dummy: &'a Dummy,
}

impl<'a> Struct<'a> {
    pub fn new(name: String, path: Vec<String>, reference: Option<String>) -> Self {
        Struct {
            name,
            field: vec![],
            properties: vec![],
            path,
            reference,
            //impls: vec![],
            _dummy: &Dummy(),
        }
    }
}

impl<'a> FindSymbol for [Struct<'a>] {
    type Item = Struct<'a>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}

impl<'a> FindSymbol for Vec<Rc<Struct<'a>>> {
    type Item = Rc<Struct<'a>>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Class<'a> {
    pub name: String,
    pub field: Vec<Object>,
    pub properties: Vec<Object>,
    pub path: Vec<String>,
    pub reference: Option<String>,
    //pub impls: Vec<Impl<'a>>,  // trait毎
    pub _dummy: &'a Dummy,
}

impl<'a> Class<'a> {
    pub fn new(name: String, path: Vec<String>, reference: Option<String>) -> Self {
        Class {
            name,
            field: vec![],
            properties: vec![],
            path,
            reference,
            //impls: vec![],
            _dummy: &Dummy(),
        }
    }
}

impl<'a> FindSymbol for [Class<'a>] {
    type Item = Class<'a>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}

impl<'a> FindSymbol for Vec<Rc<Class<'a>>> {
    type Item = Rc<Class<'a>>;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|s|s.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|s|s.name == name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Impl<'a> {
    // TODO: trait
    pub name: String,
    pub path: Vec<String>,
    pub reference: Option<String>,
    pub functions: Vec<Rc<Function<'a>>>,
}

impl<'a> Impl<'a> {
    pub fn new(name: String, path: Vec<String>, reference: Option<String>) -> Self {
        Impl {
            name,
            path,
            reference,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumDef {
    pub name: String,
    pub path: Vec<String>,
    pub reference: Option<String>,
    pub variants: Vec<Object>,
}

impl EnumDef {
    pub fn new(name: String, path: Vec<String>) -> Self {
        EnumDef {
            name,
            path,
            reference: None,
            variants: vec![],
        }
    }
}
