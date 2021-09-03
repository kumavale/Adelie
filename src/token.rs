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

    Semicolon,  // ;

    Integer(u64),  // [1-9][0-9]*

    Ident(String),  // [a-zA-Z]
    Keyword(Keywords),

    Illegal(String),
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keywords {
    Return,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub cur: usize,
    //pub line: u32,
    //pub literal: String,
}
