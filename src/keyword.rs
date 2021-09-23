use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
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
    Struct(String),
    Ptr(Box<Type>),
    _Self(String),
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Numeric {
    I32,
    Integer,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Numeric(Numeric::I32)     => write!(f, "i32"),
            Type::Numeric(Numeric::Integer) => write!(f, "{{integer}}"),
            Type::Bool      => write!(f, "bool"),
            Type::Char      => write!(f, "char"),
            Type::String    => write!(f, "string"),
            Type::Struct(n) => write!(f, "{}", n),
            Type::Ptr(t)    => write!(f, "&{}", t),
            Type::_Self(n)  => write!(f, "{}", n),
            Type::Void      => write!(f, "void"),
        }
    }
}

impl Type {
    pub fn to_ilstr(&self) -> String {
        match self {
            Type::Numeric(Numeric::I32)     => "int32".to_string(),
            Type::Numeric(Numeric::Integer) => "int32".to_string(),  // TODO: maybe unreachable
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "string".to_string(),
            Type::Struct(n) => format!("valuetype {}", n),
            Type::Ptr(t) => format!("{}&", t.to_ilstr()),
            Type::_Self(n) => n.to_string(),
            Type::Void => "void".to_string(),
        }
    }
}
