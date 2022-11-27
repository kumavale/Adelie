use std::cell::RefCell;
use std::rc::Rc;
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

    pub fn enter_namespace(&mut self, id: &str) {
        //1 let child = Rc::new(RefCell::new(NameSpace::new(id, Some(Rc::clone(&self.current_namespace)))));
        //1 //Rc::make_mut(&mut self.current_namespace).children.push(Rc::clone(&child));
        //1 //self.current_namespace.children.borrow_mut().push(Rc::clone(&child));
        //1 self.current_namespace.children.push(Rc::clone(&child));
        //1 self.current_namespace = child;
        let child = Rc::new(NameSpace::new(id, Some(Rc::clone(&self.current_namespace))));
        //self.current_namespace.children.push(Rc::clone(&child));
        Rc::make_mut(&mut self.current_namespace).children.push(Rc::clone(&child));
        self.current_namespace = Rc::clone(&child);
    }

    pub fn leave_namespace(&mut self) {
        let child = Rc::clone(&self.current_namespace);
        let parent = self.current_namespace.parent.borrow().clone();
        self.current_namespace = Rc::clone(&parent.upgrade().unwrap());
        Rc::make_mut(&mut self.current_namespace).children.push(Rc::clone(&child));
        //dbg!(&self.current_namespace);
    }
}
