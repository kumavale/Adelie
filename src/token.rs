#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Plus,      // +
    Minus,     // -
    Asterisk,  // *
    Slash,     // /
    Percent,   // %

    LParen,  // (
    RParen,  // )

    Eq,   // ==
    Lt,   // <
    Le,   // <=
    Ne,   // !=
    Gt,   // >
    Ge,   // >=

    Assign,     // =
    AddAssign,  // +=
    SubAssign,  // -=
    MulAssign,  // *=
    DivAssign,  // /=
    RemAssign,  // %=

    Semicolon,  // ;

    Integer(i32),  // -?[1-9][0-9]*

    Ident(String),  // [a-zA-Z_][0-9a-zA-Z_]*
    Keyword(Keywords),

    Illegal(String),
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keywords {
    Let,
    Return,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub cur: usize,
    //pub line: u32,
    //pub literal: String,
}
