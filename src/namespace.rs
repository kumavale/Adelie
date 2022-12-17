use crate::class::{Class, Impl, EnumDef};
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
    pub classes:   Vec<Rc<RefCell<Class<'a>>>>,
    pub enums:     Vec<Rc<EnumDef>>,
    pub functions: Vec<Rc<Function<'a>>>,
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
            classes:   vec![],
            enums:     vec![],
            functions: vec![],
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
                for cl in child.borrow().classes.iter() {
                    if cl.borrow().name == *ns {
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
                        for cl in child.borrow().classes.iter() {
                            if cl.borrow().name == *ns {
                                current_namespace = child.as_ptr();
                                continue 'tree;
                            }
                        }
                    }
                }
            }
            for cl in unsafe{ (*current_namespace).classes.iter() } {
                if cl.borrow().name == *ns {
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

    pub fn find_struct(&self, name: &str) -> Option<Rc<RefCell<Class<'a>>>> {
        // TODO: ClassKind::Struct
        self.classes
            .iter()
            .find(|item| item.borrow().name == name)
            .map(Rc::clone)
    }

    pub fn find_impl(&self, name: &str) -> Option<Rc<Impl<'a>>> {
        if let Some(st) = self.find_struct(name) {
            st.borrow().impls
                .iter()
                .find(|im| im.name == name)
                .map(Rc::clone)
        } else if let Some(cl) = self.find_class(name) {
            cl.borrow().impls
                .iter()
                .find(|item| item.name == name)
                .map(Rc::clone)
        } else {
            None
        }
    }

    pub fn find_enum(&self, name: &str) -> Option<Rc<EnumDef>> {
        self.enums
            .iter()
            .find(|item| item.name == name)
            .map(Rc::clone)
    }

    pub fn find_class(&self, name: &str) -> Option<Rc<RefCell<Class<'a>>>> {
        // TODO: ClassKind::Class
        self.classes
            .iter()
            .find(|item| item.borrow().name == name)
            .map(Rc::clone)
    }

    pub fn push_class(&mut self, c: Class<'a>) {
        self.classes.push(Rc::new(RefCell::new(c)));
    }

    pub fn push_enum(&mut self, e: EnumDef) {
        self.enums.push(Rc::new(e));
    }

    pub fn push_fn(&mut self, f: Function<'a>) {
        self.functions.push(Rc::new(f));
    }

    #[allow(dead_code)]
    pub fn full_path(&self) -> Vec<String> {
        use std::collections::VecDeque;
        let mut namespace: *const NameSpace = self;
        let mut path = VecDeque::new();
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
