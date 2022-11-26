use std::cell::RefCell;
use std::rc::{Rc, Weak};
use crate::class::{Struct, Impl};
use crate::function::Function;

/// NameSpace {
///     name: crate,
///     parent: None
///     children: [
///         NameSpace {
///             name: module1,
///             parent: Some(NameSpace { name: crate, ... } ),
///             children: [],
///             items: [
///                 fn bar();
///             ],
///         }
///     ],
///     items: [
///         fn main();
///         fn foo();
///     ],
/// }
#[derive(Clone)]
pub struct NameSpace<'a> {
    pub name: String,
    pub parent: RefCell<Weak<NameSpace<'a>>>,
    pub children: Vec<NameSpace<'a>>,
    pub functions: Vec<Rc<Function<'a>>>,
    pub structs:   Vec<Rc<Struct<'a>>>,
    pub impls:     Vec<Rc<Impl<'a>>>,
}

impl<'a> NameSpace<'a> {
    pub fn new(name: &str, parent: Option<Rc<NameSpace<'a>>>) -> Self {
        NameSpace {
            name: name.to_string(),
            parent: if let Some(parent) = parent {
                RefCell::new(Rc::downgrade(&parent))
            } else {
                RefCell::new(Weak::new())
            },
            children: vec![],
            functions: vec![],
            structs:   vec![],
            impls:     vec![],
        }
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut Self> {
        // TODO: 処理が間違っている気がする
         self.children
             .iter_mut()
             .find(|ns| ns.name == name)
    }

    pub fn find_mut_recursive(&mut self, name: &str) -> Option<&mut Self> {
        if self.name == name {
            Some(self)
        } else {
            self.children
                .iter_mut()
                .find_map(|n| n.find_mut_recursive(name))
        }
    }

    pub fn find_fn(&self, name: &str) -> Option<Rc<Function<'a>>> {
        self.functions
            .iter()
            .find(|item| item.name == name)
            .map(Rc::clone)
    }

    pub fn find_struct(&self, name: &str) -> Option<Rc<Struct<'a>>> {
        self.structs
            .iter()
            .find(|item| item.name == name)
            .map(Rc::clone)
    }

    pub fn find_impl(&self, name: &str) -> Option<Rc<Impl<'a>>> {
        self.impls
            .iter()
            .find(|item| item.name == name)
            .map(Rc::clone)
    }
}
