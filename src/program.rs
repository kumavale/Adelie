use std::borrow::Borrow;
use std::rc::Rc;
use super::class::*;
use super::function::*;
use super::namespace::*;
use super::object::*;
use crate::ast::ItemKind;

#[derive(Clone)]
pub struct Program<'a> {
    // TODO: Files { path, lines }
    pub path: &'a str,
    pub lines: Vec<&'a str>,
    //pub functions: Vec<Rc<Function<'a>>>,
    //pub structs: Vec<Rc<Struct<'a>>>,
    //pub impls: Vec<Impl<'a>>,
    pub namespace: Rc<NameSpace<'a>>,
    pub current_namespace: Rc<NameSpace<'a>>,
}

impl<'a> Program<'a> {
    pub fn new(path: &'a str, input: &'a str) -> Self {
        let namespace = Rc::new(NameSpace::new("crate", None));
        Program {
            path,
            lines: input.lines().collect(),
            //functions: vec![],
            //structs: vec![],
            namespace: Rc::clone(&namespace),
            current_namespace: namespace,
        }
    }

    pub fn find_fn(&self, name: &str) -> Option<&'a Function> {
        self.current_namespace.find_fn(name)
        //self.functions.iter().find(|f|f.name == name).map(|f|f.borrow())
    }

    pub fn exists_fn(&self, name: &str) -> bool {
        self.functions.iter().any(|f|f.name == name)
    }

    pub fn find_struct(&self, name: &str) -> Option<&Rc<Struct<'a>>> {
        self.structs.find(name)
    }

    pub fn find_mut_struct(&mut self, name: &str) -> Option<&mut Rc<Struct<'a>>> {
        self.structs.find_mut(name)
    }

    //pub fn find_impl(&self, name: &str) -> Option<&Impl> {
    //    self.impls.find(name)
    //}

    pub fn push_fn(&mut self, f: Function<'a>) {
        if self.functions.iter().any(|e|e.name == f.name) {
            panic!("the name `{}` is defined multiple times", f.name);
        } else {
            let f = Rc::new(ItemKind::Fn(f));
            Rc::make_mut(&mut self.namespace).items.push(Rc::clone(&f));
            //self.functions.push(f);
        }
    }

    pub fn push_struct(&mut self, s: Struct<'a>) {
        if self.structs.find(&s.name).is_some() {
            panic!("the name `{}` is defined multiple times", s.name);
        } else {
            self.structs.push(Rc::new(s));
        }
    }

    pub fn push_or_merge_impl(&mut self, i: Impl<'a>) {
        //1 //if let Some(dst) = self.find_mut_struct(&i.name).expect(&format!("not found {}", &i.name)) {
        //1 //let dst = self.find_mut_struct(&i.name).expect(&format!("not found {}", &i.name));
        //1 if let Some(ns) = Rc::make_mut(&mut self.namespace).find_mut(&i.name) {
        //1     ns.elements.extend_from_slice(&i.functions);
        //1     //dst.impls.first().unwrap().functions.append(&mut i.functions);
        //1 } else {
        //1     unreachable!()
        //1 }
        //1 //} else {
        //1 //    let mut ns = NameSpace::new(&i.name, None);
        //1 //    ns.elements = i.functions.clone();
        //1 //    Rc::make_mut(&mut self.namespace).children.push(ns);
        //1 //    self.find_mut_struct(&i.name).unwrap().impls.push(i);
        //1 //}

        //self.current_namespace.borrow_mut().elements.extend_from_slice(&i.functions);
        Rc::make_mut(&mut self.current_namespace).items.extend_from_slice(&i.functions);
    }

    pub fn enter_namespace(&mut self, id: &str) {
        let child = NameSpace::new(id, Some(Rc::clone(&self.namespace)));
        Rc::make_mut(&mut self.namespace).children.push(child);
        self.current_namespace = Rc::clone(&self.namespace);
    }

    pub fn leave_namespace(&mut self) {
        todo!()
    }
}
