use crate::function::Function;
use crate::keyword::RRType;
use crate::object::{EnumObject, SymbolTable, FindSymbol};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ClassKind {
    Struct,
    Class,
    NestedClass(String),  // parent name
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class<'a> {
    pub kind: ClassKind,
    pub name: String,
    pub field: SymbolTable,
    pub path: Vec<String>,
    pub reference: Option<String>,
    pub impls: Vec<Rc<Impl<'a>>>,
    /// 継承元クラス
    pub base: Option<RRType>,
    pub nested_class: Vec<Class<'a>>,
}

impl<'a> Class<'a> {
    pub fn new(kind: ClassKind, name: String, path: Vec<String>, reference: Option<String>) -> Self {
        Class {
            kind,
            name,
            field: SymbolTable::new(),
            path,
            reference,
            impls: vec![],
            base: None,
            nested_class: vec![],
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

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub path: Vec<String>,
    pub reference: Option<String>,
    pub fields: Vec<EnumObject>,
}

impl EnumDef {
    pub fn new(name: String, path: Vec<String>) -> Self {
        EnumDef {
            name,
            path,
            reference: None,
            fields: vec![],
        }
    }
}
