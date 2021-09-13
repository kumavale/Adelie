use super::keyword::*;
use super::builtin::*;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Percent,   // %

    Caret,  // ^
    Not,    // !
    And,    // &
    Or,     // |

    AndAnd,  // &&
    OrOr,    // ||

    Shl,    // <<
    Shr,    // >>

    LBrace,  // {
    RBrace,  // }
    LParen,  // (
    RParen,  // )

    EqEq,     // ==
    Lt,       // <
    Le,       // <=
    Ne,       // !=
    Gt,       // >
    Ge,       // >=
    CaretEq,  // ^=
    AndEq,    // &=
    OrEq,     // |=
    ShlEq,     // <<=
    ShrEq,     // >>=

    Assign,     // =
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=

    Dot,    // .
    Comma,  // ,
    Colon,  // :
    Semi,   // ;

    RArrow,  // ->

    Integer(i32),    // -?[1-9][0-9]*
    Char(char),      // '.'
    String(String),  // ".*"

    Ident(String),  // [a-zA-Z_][0-9a-zA-Z_]*
    Keyword(Keyword),
    Type(Type),
    Builtin(Builtin),
    Comment(CommentKind),

    Illegal(String),
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CommentKind {
    LineComment(String),
    BlockComment(String),
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
