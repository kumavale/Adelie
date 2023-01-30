use crate::builtin::*;
use crate::keyword::{Keyword, Numeric, Float, Type};
use crate::token::{Delimiter, LiteralKind, Token, TokenKind};
use std::cmp::Ordering;

pub struct Lexer<'a> {
    _input: &'a str,
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
            _input: input,
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
                self.seek(1);
                self.line += 1;
                self.col = 0;
            } else if c.is_whitespace() {
                self.seek(1);
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start = self.col;
        let tok = match self.ch {
            Some('+') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::PlusEq, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Plus, start..self.col+1, self.line)
            }
            Some('-') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::MinusEq, start..self.col+1, self.line)
                }
                Some('>') => {
                    self.seek(1);
                    Token::new(TokenKind::RArrow, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Minus, start..self.col+1, self.line)
            }
            Some('*') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::StarEq, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Star, start..self.col+1, self.line)
            }
            Some('/') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::SlashEq, start..self.col+1, self.line)
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
                    // TODO
                    //Token::new(TokenKind::Comment(s), start..self.col+1, self.line)
                    self.seek(1);
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
                    // TODO
                    //Token::new(TokenKind::Comment(s), start..self.col+1, self.line)
                    self.seek(1);
                    return self.next_token();
                }
                _ => Token::new(TokenKind::Slash, start..self.col+1, self.line)
            }
            Some('%') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::PercentEq, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Percent, start..self.col+1, self.line)
            }

            Some('^') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::CaretEq, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Caret, start..self.col+1, self.line)
            }
            Some('&') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::AndEq, start..self.col+1, self.line)
                }
                Some('&') => {
                    self.seek(1);
                    Token::new(TokenKind::AndAnd, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::And, start..self.col+1, self.line)
            }
            Some('|') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::OrEq, start..self.col+1, self.line)
                }
                Some('|') => {
                    self.seek(1);
                    Token::new(TokenKind::OrOr, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Or, start..self.col+1, self.line)
            }

            Some('#') => Token::new(TokenKind::Pound, start..self.col+1, self.line),

            Some('(') => Token::new(TokenKind::OpenDelim(Delimiter::Parenthesis),  start..self.col+1, self.line),
            Some('{') => Token::new(TokenKind::OpenDelim(Delimiter::Brace),        start..self.col+1, self.line),
            Some('[') => Token::new(TokenKind::OpenDelim(Delimiter::Bracket),      start..self.col+1, self.line),
            Some(')') => Token::new(TokenKind::CloseDelim(Delimiter::Parenthesis), start..self.col+1, self.line),
            Some('}') => Token::new(TokenKind::CloseDelim(Delimiter::Brace),       start..self.col+1, self.line),
            Some(']') => Token::new(TokenKind::CloseDelim(Delimiter::Bracket),     start..self.col+1, self.line),

            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::EqEq, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Assign, start..self.col+1, self.line)
            }
            Some('<') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Le, start..self.col+1, self.line)
                }
                Some('<') => {
                    self.seek(1);
                    match self.peek_char() {
                        Some('=') => {
                            self.seek(1);
                            Token::new(TokenKind::ShlEq, start..self.col+1, self.line)
                        }
                        _ => Token::new(TokenKind::Shl, start..self.col+1, self.line)
                    }
                }
                _ => Token::new(TokenKind::Lt, start..self.col+1, self.line),
            }
            Some('>') => Token::new(TokenKind::Gt, start..self.col+1, self.line),
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.seek(1);
                    Token::new(TokenKind::Ne, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Not, start..self.col+1, self.line),
            }

            Some('.') => {
                if self.chars
                    .get(self.read_position..self.read_position+4)
                    .map(|s|s.iter().collect::<String>())
                    .filter(|ident| *ident == "ctor" )
                    .is_some()
                {
                    self.seek(4);
                    Token::new(TokenKind::Keyword(Keyword::Ctor), start..self.col+1, self.line)
                } else {
                    Token::new(TokenKind::Dot, start..self.col+1, self.line)
                }
            }
            Some(',') => Token::new(TokenKind::Comma, start..self.col+1, self.line),
            Some(';') => Token::new(TokenKind::Semi,  start..self.col+1, self.line),
            Some(':') => match self.peek_char() {
                Some(':') => {
                    self.seek(1);
                    Token::new(TokenKind::PathSep, start..self.col+1, self.line)
                }
                _ => Token::new(TokenKind::Colon, start..self.col+1, self.line),
            }

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
                Token::new(TokenKind::Literal(LiteralKind::Char(s.parse::<char>().expect("invalid char"))), start..self.col+1, self.line)
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
                Token::new(TokenKind::Literal(LiteralKind::String(s)), start..self.col+1, self.line)
            }

            None => Token::new(TokenKind::Eof, start..self.col+1, self.line),

            Some(c) if matches!(c, 'a'..='z'|'A'..='Z'|'_') => {
                let mut ident = self.ch.unwrap().to_string();
                while self.peek_char()
                    .filter(|c| matches!(c, '0'..='9'|'a'..='z'|'A'..='Z'|'_')).is_some()
                {
                    ident.push(self.peek_char().unwrap());
                    self.seek(1);
                }
                match &*ident {
                    "assert"    => Token::new(TokenKind::Builtin(Builtin::Assert),   start..self.col+1, self.line),
                    "assert_eq" => Token::new(TokenKind::Builtin(Builtin::AssertEq), start..self.col+1, self.line),
                    "panic"     => Token::new(TokenKind::Builtin(Builtin::Panic),    start..self.col+1, self.line),
                    "print"     => Token::new(TokenKind::Builtin(Builtin::Print),    start..self.col+1, self.line),
                    "println"   => Token::new(TokenKind::Builtin(Builtin::Println),  start..self.col+1, self.line),
                    "read_line" => Token::new(TokenKind::Builtin(Builtin::ReadLine), start..self.col+1, self.line),

                    "i32"    => Token::new(TokenKind::Type(Type::Numeric(Numeric::I32)), start..self.col+1, self.line),
                    "i64"    => Token::new(TokenKind::Type(Type::Numeric(Numeric::I64)), start..self.col+1, self.line),
                    "f32"    => Token::new(TokenKind::Type(Type::Float(Float::F32)),     start..self.col+1, self.line),
                    "bool"   => Token::new(TokenKind::Type(Type::Bool),                  start..self.col+1, self.line),
                    "char"   => Token::new(TokenKind::Type(Type::Char),                  start..self.col+1, self.line),
                    "string" => Token::new(TokenKind::Type(Type::String),                start..self.col+1, self.line),

                    "as"     => Token::new(TokenKind::Keyword(Keyword::As),        start..self.col+1, self.line),
                    "Box"    => Token::new(TokenKind::Keyword(Keyword::Box),       start..self.col+1, self.line),
                    "break"  => Token::new(TokenKind::Keyword(Keyword::Break),     start..self.col+1, self.line),
                    "class"  => Token::new(TokenKind::Keyword(Keyword::Class),     start..self.col+1, self.line),
                    "else"   => Token::new(TokenKind::Keyword(Keyword::Else),      start..self.col+1, self.line),
                    "enum"   => Token::new(TokenKind::Keyword(Keyword::Enum),      start..self.col+1, self.line),
                    "extern" => Token::new(TokenKind::Keyword(Keyword::Extern),    start..self.col+1, self.line),
                    "false"  => Token::new(TokenKind::Keyword(Keyword::False),     start..self.col+1, self.line),
                    "fn"     => Token::new(TokenKind::Keyword(Keyword::Fn),        start..self.col+1, self.line),
                    "if"     => Token::new(TokenKind::Keyword(Keyword::If),        start..self.col+1, self.line),
                    "impl"   => Token::new(TokenKind::Keyword(Keyword::Impl),      start..self.col+1, self.line),
                    "let"    => Token::new(TokenKind::Keyword(Keyword::Let),       start..self.col+1, self.line),
                    "loop"   => Token::new(TokenKind::Keyword(Keyword::Loop),      start..self.col+1, self.line),
                    "mod"    => Token::new(TokenKind::Keyword(Keyword::Mod),       start..self.col+1, self.line),
                    "mut"    => Token::new(TokenKind::Keyword(Keyword::Mut),       start..self.col+1, self.line),
                    "self"   => Token::new(TokenKind::Keyword(Keyword::SelfLower), start..self.col+1, self.line),
                    "Self"   => Token::new(TokenKind::Keyword(Keyword::SelfUpper), start..self.col+1, self.line),
                    "struct" => Token::new(TokenKind::Keyword(Keyword::Struct),    start..self.col+1, self.line),
                    "true"   => Token::new(TokenKind::Keyword(Keyword::True),      start..self.col+1, self.line),
                    "return" => Token::new(TokenKind::Keyword(Keyword::Return),    start..self.col+1, self.line),
                    "while"  => Token::new(TokenKind::Keyword(Keyword::While),     start..self.col+1, self.line),
                    _ => Token::new(TokenKind::Identifier(ident), start..self.col+1, self.line)
                }
            }

            Some(n@'0'..='9') => {
                let mut num: i128 = n.to_digit(10).unwrap() as i128;
                while let Some(n@'0'..='9') = self.peek_char() {
                    num = num * 10 + n.to_digit(10).unwrap() as i128;
                    self.seek(1);
                }
                if let Some('.') = self.peek_char() {
                    self.seek(1);
                    let mut num = format!("{}.", num);
                    while let Some(n@'0'..='9') = self.peek_char() {
                        num.push(n);
                        self.seek(1);
                    }
                    Token::new(TokenKind::Literal(LiteralKind::Float(num)), start..self.col+1, self.line)
                } else {
                    Token::new(TokenKind::Literal(LiteralKind::Integer(num)), start..self.col+1, self.line)
                }
            }

            _ => Token::new(TokenKind::Unknown(self.ch.unwrap().to_string()), start..self.col+1, self.line)
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

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn tokenize_test() {
        let tests = [
            (r#""#, vec![]),
            (r#"0"#, vec!["0"]),
            (r#"123456789"#, vec!["123456789"]),
            (r#"1+2"#, vec!["1", "+", "2"]),
            (r#" 42 * 1 "#, vec!["42", "*", "1"]),
            (r#"42--1"#, vec!["42", "-", "-", "1"]),
            (r#"4/2"#, vec!["4", "/", "2"]),
            (r#"4%2"#, vec!["4", "%", "2"]),
            (r#"1(2)3"#, vec!["1", "(", "2", ")", "3"]),
            (r#"1==2"#, vec!["1", "==", "2"]),
            (r#"1<2"#, vec!["1", "<", "2"]),
            (r#"1<=2"#, vec!["1", "<=", "2"]),
            (r#"1!=2"#, vec!["1", "!=", "2"]),
            (r#"1>2"#, vec!["1", ">", "2"]),
            (r#"1>=2"#, vec!["1", ">", "=", "2"]),
            (r#"return;"#, vec!["return", ";"]),
            (r#"return 0;"#, vec!["return", "0", ";"]),
            (r#"a=1"#, vec!["a", "=", "1"]),
            (r#"z=1"#, vec!["z", "=", "1"]),
            (r#"a+=1"#, vec!["a", "+=", "1"]),
            (r#"a-=1"#, vec!["a", "-=", "1"]),
            (r#"a*=1"#, vec!["a", "*=", "1"]),
            (r#"a/=1"#, vec!["a", "/=", "1"]),
            (r#"a%=1"#, vec!["a", "%=", "1"]),
            (r#"a0"#, vec!["a0"]),
            (r#"z9"#, vec!["z9"]),
            (r#"Aa"#, vec!["Aa"]),
            (r#"Zz"#, vec!["Zz"]),
            (r#"_A"#, vec!["_A"]),
            (r#"_Z"#, vec!["_Z"]),
            (r#"__"#, vec!["__"]),
            (r#"let foo;"#, vec!["let", "foo", ";"]),
            (r#"if 1==1{}else{}"#, vec!["if", "1", "==", "1", "{", "}", "else", "{", "}"]),
            (r#"while 1<1 {}"#, vec!["while", "1", "<", "1", "{", "}"]),
            (r#"foo()"#, vec!["foo", "(", ")"]),
            (r#"foo(1,2)"#, vec!["foo", "(", "1", ",", "2", ")"]),
            (r#"fn main() {}"#, vec!["fn", "main", "(", ")", "{", "}"]),
            (r#"let a:i32;"#, vec!["let", "a", ":", "i32", ";"]),
            (r#"fn foo() -> i32 {}"#, vec!["fn", "foo", "(", ")", "->", "i32", "{", "}"]),
            (r#""str""#, vec![r#"\"str\""#]),
            (r#"let a:string;"#, vec!["let", "a", ":", "string", ";"]),
            (r#"//comment"#, vec![]),
            (r#"/**/"#, vec![]),
            (r#"/*1*/"#, vec![]),
            (r#"let a:bool;"#, vec!["let", "a", ":", "bool", ";"]),
            (r#"true"#, vec!["true"]),
            (r#"false"#, vec!["false"]),
            (r#"!true"#, vec!["!", "true"]),
            (r#"1^1"#, vec!["1", "^", "1"]),
            (r#"1&1"#, vec!["1", "&", "1"]),
            (r#"1|1"#, vec!["1", "|", "1"]),
            (r#"1^=1"#, vec!["1", "^=", "1"]),
            (r#"1&=1"#, vec!["1", "&=", "1"]),
            (r#"1|=1"#, vec!["1", "|=", "1"]),
            (r#"&a"#, vec!["&", "a"]),
            (r#"*a"#, vec!["*", "a"]),
            (r#"'a'"#, vec!["'a'"]),
            (r#"let a:char;"#, vec!["let", "a", ":", "char", ";"]),
            (r#"a as i32"#, vec!["a", "as", "i32"]),
            (r#"asuka i32"#, vec!["asuka", "i32"]),
            (r#"loop{}"#, vec!["loop", "{", "}"]),
            (r#"&&"#, vec!["&&"]),
            (r#"& &"#, vec!["&", "&"]),
            (r#"||"#, vec!["||"]),
            (r#"| |"#, vec!["|", "|"]),
            (r#"<<"#, vec!["<<"]),
            (r#"< <"#, vec!["<", "<"]),
            (r#">>"#, vec![">", ">"]),
            (r#"<<="#, vec!["<<="]),
            (r#"< <="#, vec!["<", "<="]),
            (r#"<< ="#, vec!["<<", "="]),
            (r#">>="#, vec![">", ">", "="]),
            (r#"struct a{x:i32}"#, vec!["struct", "a", "{", "x", ":", "i32", "}"]),
            (r#"impl Foo{}"#, vec!["impl", "Foo", "{", "}"]),
            (r#".ctor()"#, vec![".ctor", "(", ")"]),
            (r#"foo.bar"#, vec!["foo", ".", "bar"]),
            (r#"break"#, vec!["break"]),
            (r#"mod m{}"#, vec!["mod", "m", "{", "}"]),
            (r#"foo::bar"#, vec!["foo", "::", "bar"]),
            (r#"println!()"#, vec!["println", "!", "(", ")"]),
            (r#"let mut a;"#, vec!["let", "mut", "a", ";"]),
            (r#"let a:Box<i32>;"#, vec!["let", "a", ":", "Box", "<", "i32", ">", ";"]),
            (r#"Box::new(1)"#, vec!["Box", "::", "new", "(", "1", ")"]),
            (r#"extern{}"#, vec!["extern", "{", "}"]),
            (r#"class a{}"#, vec!["class", "a", "{", "}"]),
            (r#"class a:b{}"#, vec!["class", "a", ":", "b", "{", "}"]),
            (r#"||{}"#, vec!["||", "{", "}"]),
            (r#"|a:i32|{}"#, vec!["|", "a", ":", "i32", "|", "{", "}"]),
            (r#"let a:f32;"#, vec!["let", "a", ":", "f32", ";"]),
            (r#"enum a{}"#, vec!["enum", "a", "{", "}"]),
            (r#"let a:i64;"#, vec!["let", "a", ":", "i64", ";"]),
            (r#"#[link]"#, vec!["#", "[", "link", "]"]),
        ];
        for (input, expect) in tests {
            let mut lexer = Lexer::new(input);
            let tokens = tokenize(&mut lexer).into_iter().map(|t| t.kind.to_string()).collect::<Vec<_>>();
            assert_eq!(tokens[..tokens.len()-1], expect);
        }
    }
}
