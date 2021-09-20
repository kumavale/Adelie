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
    Comment(CommentKind),

    Unknown(String),
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind{
    Char(char),
    String(String),
    Integer(i128),
}

impl LiteralKind {
    pub fn to_string(&self) -> String {
        match self {
            LiteralKind::Char(c)    => format!("'{}'", c),
            LiteralKind::String(s)  => format!("\"{}\"", s),
            LiteralKind::Integer(i) => i.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CommentKind {
    LineComment(String),
    BlockComment(String),
}

impl CommentKind {
    pub fn to_string(&self) -> String {
        match self {
            CommentKind::LineComment(s)  => s.to_string(),
            CommentKind::BlockComment(s) => s.to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub cur: usize,
    pub line: usize,
    //pub literal: String,
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

impl TokenKind {
    pub fn to_string(&self) -> String {
        match self {
            TokenKind::Plus    => "+".to_string(),
            TokenKind::Minus   => "-".to_string(),
            TokenKind::Star    => "*".to_string(),
            TokenKind::Slash   => "/".to_string(),
            TokenKind::Percent => "%".to_string(),

            TokenKind::Caret => "^".to_string(),
            TokenKind::Not   => "!".to_string(),
            TokenKind::And   => "&".to_string(),
            TokenKind::Or    => "|".to_string(),

            TokenKind::AndAnd => "&&".to_string(),
            TokenKind::OrOr   => "||".to_string(),

            TokenKind::Shl => "<<".to_string(),
            TokenKind::Shr => ">>".to_string(),

            TokenKind::LBrace => "{".to_string(),
            TokenKind::RBrace => "}".to_string(),
            TokenKind::LParen => "(".to_string(),
            TokenKind::RParen => ")".to_string(),

            TokenKind::EqEq    => "==".to_string(),
            TokenKind::Lt      => "<".to_string(),
            TokenKind::Le      => "<=".to_string(),
            TokenKind::Ne      => "!=".to_string(),
            TokenKind::Gt      => ">".to_string(),
            TokenKind::Ge      => ">=".to_string(),
            TokenKind::CaretEq => "^=".to_string(),
            TokenKind::AndEq   => "&=".to_string(),
            TokenKind::OrEq    => "|=".to_string(),
            TokenKind::ShlEq   => "<<=".to_string(),
            TokenKind::ShrEq   => ">>=".to_string(),

            TokenKind::Assign    => "=".to_string(),
            TokenKind::PlusEq    => "+=".to_string(),
            TokenKind::MinusEq   => "-=".to_string(),
            TokenKind::StarEq    => "*=".to_string(),
            TokenKind::SlashEq   => "/=".to_string(),
            TokenKind::PercentEq => "%=".to_string(),

            TokenKind::Dot     => ".".to_string(),
            TokenKind::Comma   => ",".to_string(),
            TokenKind::Semi    => ";".to_string(),
            TokenKind::Colon   => ":".to_string(),
            TokenKind::PathSep => "::".to_string(),

            TokenKind::RArrow => "->".to_string(),

            TokenKind::Keyword(kw)    => kw.as_str().to_string(),
            TokenKind::Identifier(id) => id.to_string(),
            TokenKind::Literal(li)    => li.to_string(),
            TokenKind::Type(ty)       => ty.to_string(),
            TokenKind::Builtin(bi)    => bi.to_string(),
            TokenKind::Comment(co)    => co.to_string(),

            TokenKind::Unknown(s) => s.to_string(),
            TokenKind::Eof => "".to_string(),
        }
    }
}
