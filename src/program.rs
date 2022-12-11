use crate::ast::Attribute;
use crate::error::Errors;
use crate::namespace::NameSpace;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Clone)]
pub struct Program<'a> {
    // TODO: Files { path, lines }
    pub path: &'a str,
    pub lines: Vec<&'a str>,
    pub namespace: Rc<RefCell<NameSpace<'a>>>,
    pub current_namespace: Rc<RefCell<NameSpace<'a>>>,
    pub errors: Rc<RefCell<Errors>>,
    pub references: Vec<Attribute>,
    pub ret_address: RefCell<bool>,
}

impl<'a> Program<'a> {
    pub fn new(path: &'a str, input: &'a str, errors: Rc<RefCell<Errors>>) -> Self {
        let namespace = Rc::new(RefCell::new(NameSpace::new("crate", None)));
        Program {
            path,
            lines: input.lines().collect(),
            namespace: Rc::clone(&namespace),
            current_namespace: namespace,
            errors,
            references: vec![],
            ret_address: RefCell::new(false),
        }
    }

    pub fn enter_namespace(&mut self, id: &str) {
        let same_ns = self.current_namespace.borrow().children
            .iter()
            .find(|child| child.borrow().name == id)
            .map(Rc::clone);
        if let Some(ns) = same_ns {
            self.current_namespace = ns;
        } else {
            let child = Rc::new(RefCell::new(NameSpace::new(id, Some(Rc::clone(&self.current_namespace)))));
            self.current_namespace.borrow_mut().children.push(Rc::clone(&child));
            self.current_namespace = child;
        }
    }

    pub fn leave_namespace(&mut self) {
        let parent = Weak::clone(&self.current_namespace.borrow().parent);
        self.current_namespace = Rc::clone(&parent.upgrade().unwrap());
    }
}
