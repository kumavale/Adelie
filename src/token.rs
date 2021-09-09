use super::keyword::*;
use super::builtin::*;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Percent,   // %

    LBrace,  // {
    RBrace,  // }
    LParen,  // (
    RParen,  // )

    EqEq,  // ==
    Lt,    // <
    Le,    // <=
    Ne,    // !=
    Gt,    // >
    Ge,    // >=

    Assign,     // =
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=

    Comma,  // ,
    Colon,  // :
    Semi,   // ;

    RArrow,  // ->

    Integer(i32),    // -?[1-9][0-9]*
    String(String),  // ".*"

    Ident(String),  // [a-zA-Z_][0-9a-zA-Z_]*
    Keyword(Keyword),
    Type(Type),
    Builtin(Builtin),
    Comment(String),

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

impl Token {
    pub fn new(kind: TokenKind, cur: usize) -> Self {
        Token { kind, cur }
    }
}
