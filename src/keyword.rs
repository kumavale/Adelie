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
    Inference,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeric {
    I32,
    Inference,
}

impl Type {
    pub fn as_ilstr(&self) -> &str {
        match self {
            Type::Numeric(Numeric::I32) => "int32",
            Type::Numeric(Numeric::Inference) => "int32",
            Type::Inference => "int32",
        }
    }
}
