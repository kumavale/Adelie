#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    As,
    Else,
    False,
    Fn,
    If,
    Impl,
    Let,
    Loop,
    Struct,
    True,
    Return,
    While,
}

impl Keyword {
    pub fn as_str(&self) -> &str {
        match self {
            Keyword::As => "as",
            Keyword::Else => "else",
            Keyword::False => "false",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Impl => "impl",
            Keyword::Let => "let",
            Keyword::Loop => "loop",
            Keyword::Struct => "struct",
            Keyword::True => "true",
            Keyword::Return => "return",
            Keyword::While => "while",
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
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Numeric {
    I32,
}

impl Type {
    pub fn to_str(&self) -> String {
        match self {
            Type::Numeric(Numeric::I32) => "i32".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "String".to_string(),
            Type::Struct(n) => n.to_string(),
            Type::Ptr(t) => format!("&{}", t.to_str()),
            Type::Void => "void".to_string(),
        }
    }

    pub fn to_ilstr(&self) -> String {
        match self {
            Type::Numeric(Numeric::I32) => "int32".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::String => "string".to_string(),
            Type::Struct(n) => format!("valuetype {}", n),
            Type::Ptr(t) => format!("{}&", t.to_ilstr()),
            Type::Void => "void".to_string(),
        }
    }
}
