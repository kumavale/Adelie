use std::borrow::Borrow;
use std::rc::Rc;
use super::class::*;
use super::function::*;
use super::namespace::*;
use super::object::*;

#[derive(Clone, Debug)]
pub struct Program<'a> {
    // TODO: Files { path, lines }
    pub path: &'a str,
    pub lines: Vec<&'a str>,
    pub functions: Vec<Rc<Function<'a>>>,
    pub structs: Vec<Struct<'a>>,
    pub impls: Vec<Impl<'a>>,
    pub namespace: Rc<NameSpace<'a>>,
}

impl<'a> Program<'a> {
    pub fn new(path: &'a str, input: &'a str) -> Self {
        Program {
            path,
            lines: input.lines().collect(),
            functions: vec![],
            structs: vec![],
            impls: vec![],
            namespace: Rc::new(NameSpace::new("crate", None)),
        }
    }

    pub fn find_fn(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(|f|f.name == name).map(|f|f.borrow())
    }

    pub fn find_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.find(name)
    }

    pub fn find_impl(&self, name: &str) -> Option<&Impl> {
        self.impls.find(name)
    }

    pub fn push_fn(&mut self, f: Function<'a>) {
        if self.functions.iter().any(|e|e.name == f.name) {
            panic!("the name `{}` is defined multiple times", f.name);
        } else {
            let f = Rc::new(f);
            Rc::make_mut(&mut self.namespace).elements.push(Rc::clone(&f));
            self.functions.push(f);
        }
    }

    pub fn push_struct(&mut self, s: Struct<'a>) {
        if self.structs.find(&s.name).is_some() {
            panic!("the name `{}` is defined multiple times", s.name);
        } else {
            self.structs.push(s);
        }
    }

    pub fn push_or_merge_impl(&mut self, mut i: Impl<'a>) {
        // TODO: たぶん処理が間違っている
        if let Some(dst) = self.impls.find_mut(&i.name) {
            if let Some(ns) = Rc::make_mut(&mut self.namespace).find_mut(&i.name) {
                ns.elements.extend_from_slice(&i.functions);
                dst.functions.append(&mut i.functions);
            } else {
                unreachable!()
            }
        } else {
            let mut ns = NameSpace::new(&i.name, None);
            ns.elements = i.functions.clone();
            Rc::make_mut(&mut self.namespace).children.push(ns);
            self.impls.push(i);
        }
    }

    pub fn enter_namespace(&mut self, id: &str) {
        let child = NameSpace::new(id, Some(Rc::clone(&self.namespace)));
        Rc::make_mut(&mut self.namespace).children.push(child);
    }

    pub fn leave_namespace(&mut self) {
        todo!()
    }
}
