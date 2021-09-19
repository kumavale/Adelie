use std::cmp::Ordering;
use super::token::*;
use super::keyword::*;
use super::builtin::*;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    pub ch: Option<char>,
    pub chars: Vec<char>,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            chars: input.chars().collect(),
            line: 1,
            col: 0,
        };
        l.seek(1);
        l
    }

    fn seek(&mut self, offset: i32) {
        match offset.cmp(&0) {
            Ordering::Equal => (),  // Do nothing
            Ordering::Greater => {
                self.ch = self.peek_char();
                self.position = self.read_position;
                self.read_position += 1;
                self.col += 1;
                self.seek(offset-1);
            }
            Ordering::Less => {
                self.ch = self.peek_char();
                self.position = self.read_position;
                self.read_position -= 1;
                self.col -= 1;
                self.seek(offset+1);
            }
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.chars.len() {
            None
        } else {
            Some(self.chars[self.read_position])
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            }
            if c.is_whitespace() {
                self.seek(1);
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            Some('+') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::PlusEq, self.col, self.line)
                }
                _ => Token::new(TokenKind::Plus, self.col, self.line)
            }
            Some('-') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::MinusEq, self.col, self.line)
                }
                Some('>') => {
                    self.seek(1);
                    Token::new(TokenKind::RArrow, self.col, self.line)
                }
                _ => Token::new(TokenKind::Minus, self.col, self.line)
            }
            Some('*') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::StarEq, self.col, self.line)
                }
                _ => Token::new(TokenKind::Star, self.col, self.line)
            }
            Some('/') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::SlashEq, self.col, self.line)
                }
                Some('/') => {
                    self.seek(1);
                    let mut s = "//".to_string();
                    while let Some(c) = self.peek_char() {
                        self.seek(1);
                        if c == '\n' {
                            break;
                        }
                        s.push(c);
                    }
                    //Token::new(TokenKind::Comment(s), self.col, self.line)
                    return self.next_token();
                }
                Some('*') => {
                    self.seek(1);
                    let mut s = "/*".to_string();
                    while let Some(c) = self.peek_char() {
                        self.seek(1);
                        if c == '*' {
                            if let Some('/') = self.peek_char() {
                                self.seek(2);
                                s.push_str("*/");
                                break;
                            }
                        }
                        s.push(c);
                    }
                    //Token::new(TokenKind::Comment(s), self.col, self.line)
                    return self.next_token();
                }
                _ => Token::new(TokenKind::Slash, self.col, self.line)
            }
            Some('%') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::PercentEq, self.col, self.line)
                }
                _ => Token::new(TokenKind::Percent, self.col, self.line)
            }

            Some('^') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::CaretEq, self.col, self.line)
                }
                _ => Token::new(TokenKind::Caret, self.col, self.line)
            }
            Some('&') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::AndEq, self.col, self.line)
                }
                Some('&') => {
                    self.seek(1);
                    Token::new(TokenKind::AndAnd, self.col, self.line)
                }
                _ => Token::new(TokenKind::And, self.col, self.line)
            }
            Some('|') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::OrEq, self.col, self.line)
                }
                Some('|') => {
                    self.seek(1);
                    Token::new(TokenKind::OrOr, self.col, self.line)
                }
                _ => Token::new(TokenKind::Or, self.col, self.line)
            }

            Some('{') => Token::new(TokenKind::LBrace, self.col, self.line),
            Some('}') => Token::new(TokenKind::RBrace, self.col, self.line),
            Some('(') => Token::new(TokenKind::LParen, self.col, self.line),
            Some(')') => Token::new(TokenKind::RParen, self.col, self.line),

            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::EqEq, self.col, self.line)
                }
                _ => Token::new(TokenKind::Assign, self.col, self.line)
            }
            Some('<') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Le, self.col, self.line)
                }
                Some('<') => {
                    self.seek(1);
                    match self.peek_char() {
                        Some('=') => {
                            self.seek(1);
                            Token::new(TokenKind::ShlEq, self.col, self.line)
                        }
                        _ => Token::new(TokenKind::Shl, self.col, self.line)
                    }
                }
                _ => Token::new(TokenKind::Lt, self.col, self.line),
            }
            Some('>') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Ge, self.col, self.line)
                }
                Some('>') => {
                    self.seek(1);
                    match self.peek_char() {
                        Some('=') => {
                            self.seek(1);
                            Token::new(TokenKind::ShrEq, self.col, self.line)
                        }
                        _ => Token::new(TokenKind::Shr, self.col, self.line)
                    }
                }
                _ => Token::new(TokenKind::Gt, self.col, self.line),
            }
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Ne, self.col, self.line)
                }
                _ => Token::new(TokenKind::Not, self.col, self.line),
            }

            Some('.') => Token::new(TokenKind::Dot,   self.col, self.line),
            Some(',') => Token::new(TokenKind::Comma, self.col, self.line),
            Some(':') => Token::new(TokenKind::Colon, self.col, self.line),
            Some(';') => Token::new(TokenKind::Semi,  self.col, self.line),

            Some('\'') => {
                //todo!('\n \r \\ ...');
                let mut s = String::new();
                while let Some(c) = self.peek_char() {
                    self.seek(1);
                    if c == '\'' {
                        break;
                    }
                    s.push(c);
                }
                Token::new(TokenKind::Literal(LiteralKind::Char(s.parse::<char>().expect("invalid char"))), self.col, self.line)
            }
            Some('"') => {
                let mut s = String::new();
                while let Some(c) = self.peek_char() {
                    self.seek(1);
                    if c == '"' {
                        break;
                    }
                    s.push(c);
                }
                Token::new(TokenKind::Literal(LiteralKind::String(s)), self.col, self.line)
            }

            None => Token::new(TokenKind::Eof, self.col, self.line),

            Some('a'..='z'|'A'..='Z'|'_') => {
                let mut ident = self.ch.unwrap().to_string();
                while let Some('0'..='9'|'a'..='z'|'A'..='Z'|'_') = self.peek_char() {
                    ident.push(self.peek_char().unwrap());
                    self.seek(1);
                }
                match &*ident {
                    "print"   => Token::new(TokenKind::Builtin(Builtin::Print),   self.col, self.line),
                    "println" => Token::new(TokenKind::Builtin(Builtin::Println), self.col, self.line),

                    "i32"    => Token::new(TokenKind::Type(Type::Numeric(Numeric::I32)), self.col, self.line),
                    "bool"   => Token::new(TokenKind::Type(Type::Bool),                  self.col, self.line),
                    "char"   => Token::new(TokenKind::Type(Type::Char),                  self.col, self.line),
                    "string" => Token::new(TokenKind::Type(Type::String),                self.col, self.line),

                    "as"     => Token::new(TokenKind::Keyword(Keyword::As),      self.col, self.line),
                    "break"  => Token::new(TokenKind::Keyword(Keyword::Break),   self.col, self.line),
                    "else"   => Token::new(TokenKind::Keyword(Keyword::Else),    self.col, self.line),
                    "false"  => Token::new(TokenKind::Keyword(Keyword::False),   self.col, self.line),
                    "fn"     => Token::new(TokenKind::Keyword(Keyword::Fn),      self.col, self.line),
                    "if"     => Token::new(TokenKind::Keyword(Keyword::If),      self.col, self.line),
                    "impl"   => Token::new(TokenKind::Keyword(Keyword::Impl),    self.col, self.line),
                    "let"    => Token::new(TokenKind::Keyword(Keyword::Let),     self.col, self.line),
                    "loop"   => Token::new(TokenKind::Keyword(Keyword::Loop),    self.col, self.line),
                    "self" => Token::new(TokenKind::Keyword(Keyword::SelfLower), self.col, self.line),
                    "Self" => Token::new(TokenKind::Keyword(Keyword::SelfUpper), self.col, self.line),
                    "struct" => Token::new(TokenKind::Keyword(Keyword::Struct),  self.col, self.line),
                    "true"   => Token::new(TokenKind::Keyword(Keyword::True),    self.col, self.line),
                    "return" => Token::new(TokenKind::Keyword(Keyword::Return),  self.col, self.line),
                    "while"  => Token::new(TokenKind::Keyword(Keyword::While),   self.col, self.line),
                    _ => Token::new(TokenKind::Identifier(ident), self.col, self.line)
                }
            }

            Some(n@'0'..='9') => {
                let mut num: i128 = n.to_digit(10).unwrap() as i128;
                while let Some(n@'0'..='9') = self.peek_char() {
                    num = num * 10 + n.to_digit(10).unwrap() as i128;
                    self.seek(1);
                }
                Token::new(TokenKind::Literal(LiteralKind::Integer(num)), self.col, self.line)
            }

            _ => Token::new(TokenKind::Unknown(self.ch.unwrap().to_string()), self.col + 1, self.line)
        };

        self.seek(1);
        tok
    }
}

pub fn tokenize(l: &mut Lexer) -> Vec<Token> {
    let mut tok = vec![l.next_token()];
    while tok[tok.len()-1].kind != TokenKind::Eof {
        tok.push(l.next_token());
    }
    tok
}
