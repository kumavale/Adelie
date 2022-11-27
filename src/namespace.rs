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
#[derive(Clone, Debug)]
pub struct NameSpace<'a> {
    pub name: String,
    pub parent: Weak<RefCell<NameSpace<'a>>>,
    pub children: Vec<Rc<RefCell<NameSpace<'a>>>>,
    pub functions: Vec<Rc<Function<'a>>>,
    pub structs:   Vec<Rc<Struct<'a>>>,
    pub impls:     Vec<Rc<Impl<'a>>>,
}

impl<'a> NameSpace<'a> {
    pub fn new(name: &str, parent: Option<Rc<RefCell<NameSpace<'a>>>>) -> Self {
        NameSpace {
            name: name.to_string(),
            parent: if let Some(parent) = parent {
                Rc::downgrade(&parent)
            } else {
                Weak::new()
            },
            children: vec![],
            functions: vec![],
            structs:   vec![],
            impls:     vec![],
        }
    }

    // TODO: without `unsafe`
    pub fn find(&self, path_tree: &[String]) -> Option<&Self> {
        let mut current_namespace: *const NameSpace = self;
        'tree:for ns in path_tree {
            for child in unsafe{ (*current_namespace).children.iter() } {
                if child.borrow().name == *ns {
                    current_namespace = child.as_ptr();
                    continue 'tree;
                }
            }
            return None;
        }
        unsafe { Some(&*current_namespace) }
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

    pub fn push_fn(&mut self, f: Function<'a>) {
        if self.find_fn(&f.name).is_some() {
            panic!("the name `{}` is defined multiple times", f.name);
        }
        self.functions.push(Rc::new(f));
    }

    pub fn push_struct(&mut self, s: Struct<'a>) {
        // TODO: partial classとして処理するか？
        if self.find_struct(&s.name).is_some() {
            panic!("the name `{}` is defined multiple times", s.name);
        }
        self.structs.push(Rc::new(s));
    }

    pub fn push_impl(&mut self, i: Impl<'a>) {
        self.impls.push(Rc::new(i));
    }
}
