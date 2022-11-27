use std::cell::RefCell;
use std::rc::Rc;
use super::class::*;
use super::function::*;
use super::namespace::*;

#[derive(Clone)]
pub struct Program<'a> {
    // TODO: Files { path, lines }
    pub path: &'a str,
    pub lines: Vec<&'a str>,
    pub namespace: Rc<NameSpace<'a>>,
    pub current_namespace: Rc<NameSpace<'a>>,
}

impl<'a> Program<'a> {
    pub fn new(path: &'a str, input: &'a str) -> Self {
        let namespace = Rc::new(NameSpace::new("crate", None));
        Program {
            path,
            lines: input.lines().collect(),
            namespace: Rc::clone(&namespace),
            current_namespace: namespace,
        }
    }

    pub fn find_fn(&self, name: &str) -> Option<Rc<Function<'a>>> {
        self.current_namespace.find_fn(name)
    }

    pub fn find_struct(&self, name: &str) -> Option<Rc<Struct<'a>>> {
        self.current_namespace.find_struct(name)
    }

    pub fn push_fn(&mut self, f: Function<'a>) {
        if self.find_fn(&f.name).is_some() {
            panic!("the name `{}` is defined multiple times", f.name);
        }
        //self.current_namespace.functions.push(Rc::new(f));
        Rc::make_mut(&mut self.current_namespace).functions.push(Rc::new(f));
    }

    pub fn push_struct(&mut self, s: Struct<'a>) {
        // TODO: partial classとして処理するか？
        if self.find_struct(&s.name).is_some() {
            panic!("the name `{}` is defined multiple times", s.name);
        }
        //self.current_namespace.structs.push(Rc::new(s));
        Rc::make_mut(&mut self.current_namespace).structs.push(Rc::new(s));
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
        //self.current_namespace.impls.push(Rc::new(i));
        Rc::make_mut(&mut self.current_namespace).impls.push(Rc::new(i));
    }

    pub fn enter_namespace(&mut self, id: &str) {
        //1 let child = Rc::new(RefCell::new(NameSpace::new(id, Some(Rc::clone(&self.current_namespace)))));
        //1 //Rc::make_mut(&mut self.current_namespace).children.push(Rc::clone(&child));
        //1 //self.current_namespace.children.borrow_mut().push(Rc::clone(&child));
        //1 self.current_namespace.children.push(Rc::clone(&child));
        //1 self.current_namespace = child;
        let child = Rc::new(NameSpace::new(id, Some(Rc::clone(&self.current_namespace))));
        //self.current_namespace.children.push(*Rc::clone(&child));
        //Rc::make_mut(&mut self.current_namespace).children.push(Rc::clone(&child));
        self.current_namespace = child;
    }

    pub fn leave_namespace(&mut self) {
        let child = Rc::clone(&self.current_namespace);
        let parent = self.current_namespace.parent.borrow().clone();
        self.current_namespace = Rc::clone(&parent.upgrade().unwrap());
        Rc::make_mut(&mut self.current_namespace).children.push(Rc::clone(&child));
        //dbg!(&self.current_namespace);
    }
}
