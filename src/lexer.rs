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
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            chars: input.chars().collect(),
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
                self.seek(offset-1);
            }
            Ordering::Less => {
                self.ch = self.peek_char();
                self.position = self.read_position;
                self.read_position -= 1;
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
            if c.is_ascii_whitespace() {
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
                    Token::new(TokenKind::PlusEq, self.read_position)
                }
                _ => Token::new(TokenKind::Plus, self.read_position)
            }
            Some('-') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::MinusEq, self.read_position)
                }
                Some('>') => {
                    self.seek(1);
                    Token::new(TokenKind::RArrow, self.read_position)
                }
                _ => Token::new(TokenKind::Minus, self.read_position)
            }
            Some('*') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::StarEq, self.read_position)
                }
                _ => Token::new(TokenKind::Star, self.read_position)
            }
            Some('/') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::SlashEq, self.read_position)
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
                    //Token::new(TokenKind::Comment(s), self.read_position)
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
                    //Token::new(TokenKind::Comment(s), self.read_position)
                    return self.next_token();
                }
                _ => Token::new(TokenKind::Slash, self.read_position)
            }
            Some('%') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::PercentEq, self.read_position)
                }
                _ => Token::new(TokenKind::Percent, self.read_position)
            }

            Some('^') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::CaretEq, self.read_position)
                }
                _ => Token::new(TokenKind::Caret, self.read_position)
            }
            Some('&') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::AndEq, self.read_position)
                }
                Some('&') => {
                    self.seek(1);
                    Token::new(TokenKind::AndAnd, self.read_position)
                }
                _ => Token::new(TokenKind::And, self.read_position)
            }
            Some('|') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::OrEq, self.read_position)
                }
                Some('|') => {
                    self.seek(1);
                    Token::new(TokenKind::OrOr, self.read_position)
                }
                _ => Token::new(TokenKind::Or, self.read_position)
            }

            Some('{') => Token::new(TokenKind::LBrace, self.read_position),
            Some('}') => Token::new(TokenKind::RBrace, self.read_position),
            Some('(') => Token::new(TokenKind::LParen, self.read_position),
            Some(')') => Token::new(TokenKind::RParen, self.read_position),

            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::EqEq, self.read_position)
                }
                _ => Token::new(TokenKind::Assign, self.read_position)
            }
            Some('<') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Le, self.read_position)
                }
                _ => Token::new(TokenKind::Lt, self.read_position),
            }
            Some('>') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Ge, self.read_position)
                }
                _ => Token::new(TokenKind::Gt, self.read_position),
            }
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Ne, self.read_position)
                }
                _ => Token::new(TokenKind::Not, self.read_position),
            }

            Some(',') => Token::new(TokenKind::Comma, self.read_position),
            Some(':') => Token::new(TokenKind::Colon, self.read_position),
            Some(';') => Token::new(TokenKind::Semi,  self.read_position),

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
                Token::new(TokenKind::Char(s.parse::<char>().expect("invalid char")), self.read_position)
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
                Token::new(TokenKind::String(s), self.read_position)
            }

            None => Token::new(TokenKind::Eof, self.read_position),

            Some('a'..='z'|'A'..='Z'|'_') => {
                let mut ident = self.ch.unwrap().to_string();
                while let Some('0'..='9'|'a'..='z'|'A'..='Z'|'_') = self.peek_char() {
                    ident.push(self.peek_char().unwrap());
                    self.seek(1);
                }
                match &*ident {
                    "print"   => Token::new(TokenKind::Builtin(Builtin::Print),   self.read_position),
                    "println" => Token::new(TokenKind::Builtin(Builtin::Println), self.read_position),

                    "i32"    => Token::new(TokenKind::Type(Type::Numeric(Numeric::I32)), self.read_position),
                    "bool"   => Token::new(TokenKind::Type(Type::Bool),                  self.read_position),
                    "char"   => Token::new(TokenKind::Type(Type::Char),                  self.read_position),
                    "String" => Token::new(TokenKind::Type(Type::String),                self.read_position),

                    "as"     => Token::new(TokenKind::Keyword(Keyword::As),     self.read_position),
                    "else"   => Token::new(TokenKind::Keyword(Keyword::Else),   self.read_position),
                    "false"  => Token::new(TokenKind::Keyword(Keyword::False),  self.read_position),
                    "fn"     => Token::new(TokenKind::Keyword(Keyword::Fn),     self.read_position),
                    "if"     => Token::new(TokenKind::Keyword(Keyword::If),     self.read_position),
                    "let"    => Token::new(TokenKind::Keyword(Keyword::Let),    self.read_position),
                    "loop"   => Token::new(TokenKind::Keyword(Keyword::Loop),   self.read_position),
                    "true"   => Token::new(TokenKind::Keyword(Keyword::True),   self.read_position),
                    "return" => Token::new(TokenKind::Keyword(Keyword::Return), self.read_position),
                    "while"  => Token::new(TokenKind::Keyword(Keyword::While),  self.read_position),
                    _ => Token::new(TokenKind::Ident(ident), self.read_position)
                }
            }

            Some(n@'0'..='9') => {
                let mut num = n.to_digit(10).unwrap();
                while let Some(n@'0'..='9') = self.peek_char() {
                    num = num * 10 + n.to_digit(10).unwrap();
                    self.seek(1);
                }
                Token::new(TokenKind::Integer(num as i32), self.read_position)
            }

            _ => Token::new(TokenKind::Illegal(self.ch.unwrap().to_string()), self.read_position + 1)
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
