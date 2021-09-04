use super::token::*;

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
        self.ch = self.peek_char();
        self.position = self.read_position;
        if offset < 0 {
            self.read_position -= offset.abs() as usize;
        } else if offset > 0 {
            self.read_position += offset as usize;
        } else {
            // Do nothing
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
            Some('+') => new_token(TokenKind::Plus,     self.read_position),
            Some('-') => new_token(TokenKind::Minus,    self.read_position),
            Some('*') => new_token(TokenKind::Asterisk, self.read_position),
            Some('/') => new_token(TokenKind::Slash,    self.read_position),
            Some('%') => new_token(TokenKind::Percent,  self.read_position),

            Some('(') => new_token(TokenKind::LParen, self.read_position),
            Some(')') => new_token(TokenKind::RParen, self.read_position),

            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    new_token(TokenKind::Eq, self.read_position)
                }
                _ => new_token(TokenKind::Assign, self.read_position)
            }
            Some('<') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    new_token(TokenKind::Le, self.read_position)
                }
                _ => new_token(TokenKind::Lt, self.read_position),
            }
            Some('>') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    new_token(TokenKind::Ge, self.read_position)
                }
                _ => new_token(TokenKind::Gt, self.read_position),
            }
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    new_token(TokenKind::Ne, self.read_position)
                }
                _ => todo!("Not"),
            }

            Some(';') => new_token(TokenKind::Semicolon, self.read_position),

            None => new_token(TokenKind::Eof, self.read_position),

            Some('a'..='z') => {
                if self.chars[self.position..].iter().collect::<String>().starts_with("return") {
                    self.seek(5);
                    new_token(TokenKind::Keyword(Keywords::Return), self.read_position)
                } else {
                    new_token(TokenKind::Ident(self.ch.unwrap().to_string()), self.read_position)
                }
            }

            Some('0'..='9') => {
                let mut num = char2num(&self.ch);
                while let Some('0'..='9') = self.peek_char() {
                    num = num * 10 + char2num(&self.peek_char());
                    self.seek(1);
                }
                new_token(TokenKind::Integer(num), self.read_position)
            }

            _ => new_token(TokenKind::Illegal(self.ch.unwrap().to_string()), self.read_position + 1)
        };

        self.seek(1);
        tok
    }
}

fn new_token(kind: TokenKind, cur: usize) -> Token {
    Token { kind, cur }
}

fn char2num(ch: &Option<char>) -> u64 {
    ch.unwrap() as u64 - 48
}

pub fn tokenize(l: &mut Lexer) -> Vec<Token> {
    let mut tok = vec![l.next_token()];
    while tok[tok.len()-1].kind != TokenKind::Eof {
        tok.push(l.next_token());
    }
    tok
}
