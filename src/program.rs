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

    pub fn find_fn(&self, name: &str) -> Option<&Function> {
        self.functions.find(name)
    }

    pub fn find_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.find(name)
    }

    pub fn find_impl(&self, name: &str) -> Option<&Impl> {
        self.impls.find(name)
    }

    pub fn push_fn(&mut self, f: Function) {
        if let Some(dst) = self.functions.find(&f.name) {
            panic!("the name `{}` is defined multiple times", f.name);
        } else {
            self.functions.push(f);
        }
    }

    pub fn push_struct(&mut self, s: Struct) {
        if let Some(dst) = self.structs.find(&s.name) {
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
