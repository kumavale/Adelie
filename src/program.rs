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
        }
    }

    pub fn enter_namespace(&mut self, id: &str) {
        let child = Rc::new(RefCell::new(NameSpace::new(id, Some(Rc::clone(&self.current_namespace)))));
        self.current_namespace.borrow_mut().children.push(Rc::clone(&child));
        self.current_namespace = child;
    }

    pub fn leave_namespace(&mut self) {
        let parent = Weak::clone(&self.current_namespace.borrow().parent);
        self.current_namespace = Rc::clone(&parent.upgrade().unwrap());
    }
}
