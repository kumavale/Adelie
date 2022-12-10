use std::cell::{Ref, RefMut, RefCell};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    As,
    Box,
    Class,
    Ctor,
    Break,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    If,
    Impl,
    Let,
    Loop,
    Mod,
    Mut,
    SelfLower,
    SelfUpper,
    Struct,
    True,
    Return,
    While,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::As        => write!(f, "as"),
            Keyword::Box       => write!(f, "Box"),
            Keyword::Break     => write!(f, "break"),
            Keyword::Class     => write!(f, "class"),
            Keyword::Ctor      => write!(f, ".ctor"),
            Keyword::Else      => write!(f, "else"),
            Keyword::Enum      => write!(f, "enum"),
            Keyword::Extern    => write!(f, "extern"),
            Keyword::False     => write!(f, "false"),
            Keyword::Fn        => write!(f, "fn"),
            Keyword::If        => write!(f, "if"),
            Keyword::Impl      => write!(f, "impl"),
            Keyword::Let       => write!(f, "let"),
            Keyword::Loop      => write!(f, "loop"),
            Keyword::Mod       => write!(f, "mod"),
            Keyword::Mut       => write!(f, "mut"),
            Keyword::SelfLower => write!(f, "self"),
            Keyword::SelfUpper => write!(f, "Self"),
            Keyword::Struct    => write!(f, "struct"),
            Keyword::True      => write!(f, "true"),
            Keyword::Return    => write!(f, "return"),
            Keyword::While     => write!(f, "while"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Numeric(Numeric),
    Bool,
    Box(Box<Type>),
    Char,
    String,
    Struct(Vec<String>, String, bool),  // (path, name, is_mutable)
    _Self(Vec<String>, String, bool),   // (path, name, is_mutable)
    Ptr(Box<Type>),
    Void,

    /// enum, struct or class
    RRIdent(Vec<String>, String),  // (path, name) //pathは将来的には要らないかも
    RRBox(RRType),
    RRPtr(RRType),
}

impl Type {
    pub fn into_mutable(self) -> Type {
        match self {
            Type::Struct(path, name, _) => Type::Struct(path, name, true),
            Type::_Self(path, name, _)  => Type::_Self(path, name, true),
            t => t,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Numeric {
    I32,
    Integer,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Numeric(Numeric::I32)     => write!(f, "i32"),
            Type::Numeric(Numeric::Integer) => write!(f, "{{integer}}"),
            Type::Bool            => write!(f, "bool"),
            Type::Box(t)          => write!(f, "Box<{}>", t),
            Type::Char            => write!(f, "char"),
            Type::String          => write!(f, "string"),
            Type::Struct(_, n, _) => write!(f, "{}", n),
            Type::Ptr(t)          => write!(f, "&{}", t),
            Type::_Self(_, n, _)  => write!(f, "{}", n),
            Type::Void            => write!(f, "void"),
            Type::RRIdent(_, n)   => write!(f, "RRIdent<{}>", n),
            Type::RRBox(t)        => write!(f, "RRBox<{}>", t.borrow()),
            Type::RRPtr(t)        => write!(f, "RR&{}", t.borrow()),
        }
    }
}

impl Type {
    pub fn to_ilstr(&self) -> String {
        match self {
            Type::Numeric(n)      => n.to_ilstr(),
            Type::Bool            => "bool".to_string(),
            Type::Box(_)          => "object".to_string(),
            Type::Char            => "char".to_string(),
            Type::String          => "string".to_string(),
            Type::Struct(_, n, _) => format!("valuetype {}", n),
            Type::Ptr(t)          => format!("{}&", t.to_ilstr()),
            Type::_Self(_, n, _)  => n.to_string(),
            Type::Void            => "void".to_string(),
            Type::RRIdent(..) |
            Type::RRBox(..)   |
            Type::RRPtr(..)   => panic!("cannot to ilstr: {}", &self),
        }
    }
}

impl Numeric {
    pub fn to_ilstr(&self) -> String {
        match self {
            Numeric::I32     => "int32".to_string(),
            Numeric::Integer => "int32".to_string(),  // TODO: maybe unreachable
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct RRType {
    inner: Rc<RefCell<Type>>,
}

impl Deref for RRType {
    type Target = Rc<RefCell<Type>>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for RRType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.borrow().hash(state);
    }
}
impl Clone for RRType {
    fn clone(&self) -> Self {
        RRType {
            inner: self.inner.clone(),
        }
    }
}
impl RRType {
    pub fn new(inner: Type) -> Self {
        RRType {
            inner: Rc::new(RefCell::new(inner)),
        }
    }
    pub fn borrow(&self) -> Ref<'_, Type> {
        self.inner.borrow()
    }
    pub fn borrow_mut(&mut self) -> RefMut<'_, Type> {
        self.inner.borrow_mut()
    }
}
