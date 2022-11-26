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
    pub children: Vec<NameSpace<'a>>,
    pub elements: Vec<Rc<Function<'a>>>,
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
            elements: vec![],
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
}
