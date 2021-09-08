#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Else,
    Fn,
    If,
    Let,
    Return,
    While,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    Numeric(Numeric),
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
            Type::String => "String",
            Type::Void => "void",
        }
    }
    pub fn as_ilstr(&self) -> &str {
        match self {
            Type::Numeric(Numeric::I32) => "int32",
            Type::String => "string",
            Type::Void => "void",
        }
    }
}
