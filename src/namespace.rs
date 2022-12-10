use crate::class::{Struct, Impl, EnumDef};
use crate::function::Function;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

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
    pub enums:     Vec<Rc<EnumDef>>,
    pub is_foreign: bool,
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
            enums:     vec![],
            is_foreign: false,
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
                for st in child.borrow().structs.iter() {
                    if st.name == *ns {
                        current_namespace = child.as_ptr();
                        continue 'tree;
                    }
                }
                if child.borrow().is_foreign {
                    for child in &child.borrow().children {
                        if child.borrow().name == *ns {
                            current_namespace = child.as_ptr();
                            continue 'tree;
                        }
                        for st in child.borrow().structs.iter() {
                            if st.name == *ns {
                                current_namespace = child.as_ptr();
                                continue 'tree;
                            }
                        }
                    }
                }
            }
            for st in unsafe{ (*current_namespace).structs.iter() } {
                if st.name == *ns {
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

    pub fn find_enum(&self, name: &str) -> Option<Rc<EnumDef>> {
        self.enums
            .iter()
            .find(|item| item.name == name)
            .map(Rc::clone)
    }

    pub fn push_fn(&mut self, f: Function<'a>) {
        self.functions.push(Rc::new(f));
    }

    pub fn push_struct(&mut self, s: Struct<'a>) {
        self.structs.push(Rc::new(s));
    }

    pub fn push_impl(&mut self, i: Impl<'a>) {
        self.impls.push(Rc::new(i));
    }

    pub fn push_enum(&mut self, e: EnumDef) {
        self.enums.push(Rc::new(e));
    }

    pub fn reference(&self) -> Option<String> {
        let mut namespace: *const NameSpace = self;
        unsafe {
            while let Some(parent) = (*namespace).parent.upgrade() {
                let name = (*namespace).name.to_string();
                if parent.borrow().name == "crate" {
                    return Some(name);
                } else {
                    namespace = parent.as_ptr();
                }
            }
        }
        None
    }

    #[allow(dead_code)]
    pub fn full_path(&self) -> Vec<String> {
        use std::collections::VecDeque;
        let mut namespace: *const NameSpace = self;
        let mut path = VecDeque::new();
        path.push_front("crate".to_string());
        unsafe {
            path.push_front((*namespace).name.to_string());
            while let Some(parent) = (*namespace).parent.upgrade() {
                path.push_front(parent.borrow().name.to_string());
                namespace = parent.as_ptr();
            }
        }
        Vec::from(path)
    }
}
