#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Plus,          // +
    Minus,         // -
    Asterisk,      // *
    Slash,         // /
    Percent,       // /
    Integer(u64),  // [1-9][0-9]*

    Illegal(String),
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub cur: usize,
    //pub line: u32,
    //pub literal: String,
}
