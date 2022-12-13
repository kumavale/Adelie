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
    Get,
    If,
    Impl,
    Let,
    Loop,
    Mod,
    Mut,
    SelfLower,
    SelfUpper,
    Set,
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
            Keyword::Get       => write!(f, "get"),
            Keyword::If        => write!(f, "if"),
            Keyword::Impl      => write!(f, "impl"),
            Keyword::Let       => write!(f, "let"),
            Keyword::Loop      => write!(f, "loop"),
            Keyword::Mod       => write!(f, "mod"),
            Keyword::Mut       => write!(f, "mut"),
            Keyword::SelfLower => write!(f, "self"),
            Keyword::SelfUpper => write!(f, "Self"),
            Keyword::Set       => write!(f, "set"),
            Keyword::Struct    => write!(f, "struct"),
            Keyword::True      => write!(f, "true"),
            Keyword::Return    => write!(f, "return"),
            Keyword::While     => write!(f, "while"),
        }
    }
}

#[allow(clippy::derive_hash_xor_eq)]
#[derive(Clone, Debug, Eq, Hash)]
pub enum Type {
    Numeric(Numeric),
    Bool,
    Char,
    String,
    // TODO: Rc<RefCell<Struct<'a>>>を持たせることを検討
    Struct(Option<String>, Vec<String>, String, bool),  // (dll, path, name, is_mutable)
    _Self(Vec<String>, String, bool),   // (path, name, is_mutable)
    Enum(Option<String>, Vec<String>, String),  // (dll, path, name)
    // TODO: Rc<RefCell<Class<'a>>>を持たせることを検討
    Class(Option<String>, Vec<String>, String, Option<RRType>, Option<String>, bool),  // (dll, path, name, base, parent_name, is_mutable)
    Box(RRType),
    Ptr(RRType),
    Void,

    /// enum, struct or class
    RRIdent(Vec<String>, String),  // (path, name) //pathは将来的には要らないかも
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Numeric(nl), Type::Numeric(nr)) => nl == nr,
            (Type::Bool, Type::Bool) => true,
            (Type::Char, Type::Char) => true,
            (Type::String, Type::String) => true,
            (Type::Struct(_, pl, nl, _), Type::Struct(_, pr, nr,  _)) => pl == pr && nl == nr,
            (Type::_Self(..), Type::_Self(..)) => true,
            (Type::Enum(_, pl, nl), Type::Enum(_, pr, nr)) => pl == pr && nl == nr,
            (Type::Class(_, pl, nl, ..), Type::Class(_, pr, nr, ..)) => pl == pr && nl == nr,
            (Type::Box(l), Type::Box(r)) => l == r,
            (Type::Ptr(l), Type::Ptr(r)) => l == r,
            (Type::Void, Type::Void) => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn into_mutable(self) -> Type {
        match self {
            Type::Struct(r, p, n, _)       => Type::Struct(r, p, n, true),
            Type::_Self(p, n, _)           => Type::_Self(p, n, true),
            Type::Class(r, p, n, b, pn, _) => Type::Class(r, p, n, b, pn, true),
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
            Type::Bool               => write!(f, "bool"),
            Type::Char               => write!(f, "char"),
            Type::String             => write!(f, "string"),
            Type::Struct(.., n, _)   => write!(f, "{}", n),
            Type::_Self(_, n, _)     => write!(f, "{}", n),
            Type::Enum(_, _, n)      => write!(f, "{}", n),
            Type::Class(_, _, n, ..) => write!(f, "{}", n),
            Type::Box(t)             => write!(f, "Box<{}>", t.borrow()),
            Type::Ptr(t)             => write!(f, "&{}", t.borrow()),
            Type::Void               => write!(f, "void"),
            Type::RRIdent(_, n)      => write!(f, "RRIdent<{}>", n),
        }
    }
}

impl Type {
    pub fn to_ilstr(&self) -> String {
        match self {
            Type::Numeric(n)      => n.to_ilstr(),
            Type::Bool            => "bool".to_string(),
            Type::Char            => "char".to_string(),
            Type::String          => "string".to_string(),
            Type::_Self(_, n, _)  => format!("valuetype {}", n),
            Type::Struct(r, p, n, _) |
            Type::Enum(r, p, n)   => {
                if let Some(r) = r {
                    format!("valuetype [{}]{}.{}", r, p.join("."), n)
                } else {
                    format!("valuetype {}", n)
                }
            }
            Type::Class(r, p, n, _, pn, _) => {
                if let Some(r) = r {
                    if let Some(pn) = pn {
                        format!("class [{}]{}.{}/{}", r, p.join("."), pn, n)
                    } else {
                        format!("class [{}]{}.{}", r, p.join("."), n)
                    }
                } else {
                    format!("class {}", n)
                }
            }
            Type::Box(_)          => "object".to_string(),
            Type::Ptr(t)          => format!("{}&", t.borrow().to_ilstr()),
            Type::Void            => "void".to_string(),
            Type::RRIdent(..)     => panic!("cannot to ilstr: {}", &self),
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
