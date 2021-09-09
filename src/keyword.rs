#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Else,
    False,
    Fn,
    If,
    Let,
    True,
    Return,
    While,
}

impl Keyword {
    pub fn as_str(&self) -> &str {
        match self {
            Keyword::Else => "else",
            Keyword::False => "false",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Let => "let",
            Keyword::True => "true",
            Keyword::Return => "return",
            Keyword::While => "while",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    Numeric(Numeric),
    Bool,
    String,
    Void,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeric {
    I32,
}

impl Type {
    pub fn as_str(&self) -> &str {
        match self {
            Type::Numeric(Numeric::I32) => "i32",
            Type::Bool => "bool",
            Type::String => "String",
            Type::Void => "void",
        }
    }

    pub fn as_ilstr(&self) -> &str {
        match self {
            Type::Numeric(Numeric::I32) => "int32",
            Type::Bool => "bool",
            Type::String => "string",
            Type::Void => "void",
        }
    }
}
