use std::cell::RefCell;
use std::rc::{Rc, Weak};
use crate::ast::{Item, ItemKind};

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
    pub items: Vec<Rc<dyn Item<'a>>>,
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
            items: vec![],
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

    pub fn find_fn(&self, name: &str) -> Option<Rc<dyn Item<'a>>> {
        self.items
            .iter()
            .find(|item| {
                if let ItemKind::Fn(item_fn) = item.kind() {
                    item_fn.name == name
                } else {
                    false
                }
            })
            .map(|item| Rc::clone(item))
    }
}
