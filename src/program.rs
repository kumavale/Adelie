use super::class::*;
use super::object::*;
use super::function::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
    pub impls: Vec<Impl>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            functions: vec![],
            structs: vec![],
            impls: vec![],
        }
    }

    pub fn find_function(&self, name: &str) -> Option<&Function> {
        self.functions.find(name)
    }

    pub fn find_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.find(name)
    }

    pub fn push_struct(&mut self, mut s: Struct) {
        if let Some(dst) = self.structs.find_mut(&s.name) {
            panic!("the name `{}` is defined multiple times", s.name);
        } else {
            self.structs.push(s);
        }
    }

    pub fn push_or_merge_impl(&mut self, mut i: Impl) {
        if let Some(dst) = self.impls.find_mut(&i.name) {
            dst.functions.append(&mut i.functions);
        } else {
            self.impls.push(i);
        }
    }
}
