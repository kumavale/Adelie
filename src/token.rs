use crate::builtin::*;
use crate::keyword::{Keyword, Type};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
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

    Pound,  // #

    /// An opening delimiter (e.g., `{`).
    OpenDelim(Delimiter),
    /// An closing delimiter (e.g., `}`).
    CloseDelim(Delimiter),

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

    Dot,      // .
    Comma,    // ,
    Semi,     // ;
    Colon,    // :
    PathSep,  // ::

    RArrow,  // ->

    Keyword(Keyword),
    Identifier(String),  // [a-zA-Z_][0-9a-zA-Z_]*
    Literal(LiteralKind),
    Type(Type),
    Builtin(Builtin),

    #[allow(dead_code)]
    Comment(CommentKind),  // TODO

    Unknown(String),
    Eof,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind{
    Char(char),
    String(String),
    Integer(i128),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralKind::Char(c)    => write!(f, "'{}'", c),
            LiteralKind::String(s)  => write!(f, "\\\"{}\\\"", s),
            LiteralKind::Integer(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommentKind {
    #[allow(dead_code)]
    LineComment(String),  // TODO
    #[allow(dead_code)]
    BlockComment(String),  // TODO
}

impl fmt::Display for CommentKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CommentKind::LineComment(s)  => write!(f, "{}", s),
            CommentKind::BlockComment(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub cur: usize,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, cur: usize, line: usize) -> Self {
        Token {
            kind,
            cur,
            line,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Plus    => write!(f, "+"),
            TokenKind::Minus   => write!(f, "-"),
            TokenKind::Star    => write!(f, "*"),
            TokenKind::Slash   => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),

            TokenKind::Caret => write!(f, "^"),
            TokenKind::Not   => write!(f, "!"),
            TokenKind::And   => write!(f, "&"),
            TokenKind::Or    => write!(f, "|"),

            TokenKind::AndAnd => write!(f, "&&"),
            TokenKind::OrOr   => write!(f, "||"),

            TokenKind::Shl => write!(f, "<<"),
            TokenKind::Shr => write!(f, ">>"),

            TokenKind::Pound => write!(f, "#"),

            TokenKind::OpenDelim(Delimiter::Parenthesis)  => write!(f, "("),
            TokenKind::OpenDelim(Delimiter::Brace)        => write!(f, "{{"),
            TokenKind::OpenDelim(Delimiter::Bracket)      => write!(f, "["),
            TokenKind::CloseDelim(Delimiter::Parenthesis) => write!(f, ")"),
            TokenKind::CloseDelim(Delimiter::Brace)       => write!(f, "}}"),
            TokenKind::CloseDelim(Delimiter::Bracket)     => write!(f, "]"),

            TokenKind::EqEq    => write!(f, "=="),
            TokenKind::Lt      => write!(f, "<"),
            TokenKind::Le      => write!(f, "<="),
            TokenKind::Ne      => write!(f, "!="),
            TokenKind::Gt      => write!(f, ">"),
            TokenKind::Ge      => write!(f, ">="),
            TokenKind::CaretEq => write!(f, "^="),
            TokenKind::AndEq   => write!(f, "&="),
            TokenKind::OrEq    => write!(f, "|="),
            TokenKind::ShlEq   => write!(f, "<<="),
            TokenKind::ShrEq   => write!(f, ">>="),

            TokenKind::Assign    => write!(f, "="),
            TokenKind::PlusEq    => write!(f, "+="),
            TokenKind::MinusEq   => write!(f, "-="),
            TokenKind::StarEq    => write!(f, "*="),
            TokenKind::SlashEq   => write!(f, "/="),
            TokenKind::PercentEq => write!(f, "%="),

            TokenKind::Dot     => write!(f, "."),
            TokenKind::Comma   => write!(f, ","),
            TokenKind::Semi    => write!(f, ";"),
            TokenKind::Colon   => write!(f, ":"),
            TokenKind::PathSep => write!(f, "::"),

            TokenKind::RArrow => write!(f, "->"),

            TokenKind::Keyword(kw)    => write!(f, "{}", kw),
            TokenKind::Identifier(id) => write!(f, "{}", id),
            TokenKind::Literal(li)    => write!(f, "{}", li),
            TokenKind::Type(ty)       => write!(f, "{}", ty),
            TokenKind::Builtin(bi)    => write!(f, "{}", bi),
            TokenKind::Comment(co)    => write!(f, "{}", co),

            TokenKind::Unknown(s) => write!(f, "{}", s),
            TokenKind::Eof => write!(f, ""),
        }
    }
}
