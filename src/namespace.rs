use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use super::function::*;

/// NameSpace {
///     name: crate,
///     parent: None
///     children: [
///         NameSpace {
///             name: module1,
///             parent: Some(NameSpace { name: crate, ... } ),
///             children: [],
///             elements: [
///                 fn bar();
///             ],
///         }
///     ],
///     elements: [
///         fn main();
///         fn foo();
///     ],
/// }
#[derive(Clone, Debug)]
pub struct NameSpace<'a> {
    pub name: String,
    pub parent: RefCell<Weak<NameSpace<'a>>>,
    //pub children: RefCell<Vec<Rc<NameSpace<'a>>>>,
    //pub children: RefCell<Vec<NameSpace<'a>>>,
    pub children: Vec<NameSpace<'a>>,
    pub elements: Vec<Rc<Function<'a>>>,
}

impl<'a> NameSpace<'a> {
    pub fn new(name: &str, parent: Option<Rc<NameSpace<'a>>>) -> Self {
        NameSpace {
            name: name.to_string(),
            parent: RefCell::new(Weak::new()),
            //parent: if let Some(parent) = parent {
            //    Rc::downgrade(&parent)
            //} else {
            //    Weak::new()
            //},
            //children: RefCell::new(vec![]),
            children: vec![],
            elements: vec![],
        }
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut Self> {
        // TODO: 処理が間違っている気がする
         self.children
             //.borrow_mut()
             .iter_mut()
             .find(|ns| ns.name == name)
             //.map(|ns|Rc::<NameSpace<'a>>::get_mut(ns).unwrap())
    }

    pub fn find_mut_recursive(&mut self, name: &str) -> Option<&mut Self> {
        //1 if self.name == name {
        //1     Some(self)
        //1 } else {
        //1     self.children
        //1         .borrow_mut()
        //1         .iter_mut()
        //1         .find_map(|n| n.find_mut_recursive(name))
        //1 }
        todo!()
    }
}
