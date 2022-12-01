use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Keyword {
    As,
    Break,
    Else,
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
            Keyword::Break     => write!(f, "break"),
            Keyword::Else      => write!(f, "else"),
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

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Numeric(Numeric),
    Bool,
    Char,
    String,
    Struct(Vec<String>, String, bool),  // (path, name, is_mutable)
    Ptr(Box<Type>),
    _Self(String, bool),
    Void,
}

impl Type {
    pub fn to_mutable(self) -> Type {
        match self {
            Type::Struct(path, name, _) => Type::Struct(path, name, true),
            Type::_Self(name, _) => Type::_Self(name, true),
            t => t,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
            Type::Char            => write!(f, "char"),
            Type::String          => write!(f, "string"),
            Type::Struct(_, n, _) => write!(f, "{}", n),
            Type::Ptr(t)          => write!(f, "&{}", t),
            Type::_Self(n, _)     => write!(f, "{}", n),
            Type::Void            => write!(f, "void"),
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
            Type::Struct(_, n, _) => format!("valuetype {}", n),
            Type::Ptr(t)          => format!("{}&", t.to_ilstr()),
            Type::_Self(n, _)     => n.to_string(),
            Type::Void            => "void".to_string(),
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
