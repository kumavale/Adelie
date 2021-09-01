use super::token::*;

/// EBNF
///
/// expr    : mul ( '+' mul | '-' mul ) * ;
/// mul     : primary ( '*' primary | '/' primary | '%' primary ) * ;
/// primary : dec_digit ;
///
/// dec_digit   : '0' | nonzero_dec ;
/// nonzero_dec : '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
///

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOpKind {
    Add,  // +
    Sub,  // -
    Mul,  // *
    Div,  // /
    Rem,  // %
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Integer(u64),  // [1-9][0-9]*
    BinaryOp {
        kind: BinaryOpKind,
        lhs: Option<Box<Node>>,
        rhs: Option<Box<Node>>,
    },
}

#[derive(Debug)]
struct Tokens<'a> {
    tokens: &'a [Token],
    idx: usize,
}

pub fn gen_ast(tokens: &[Token]) -> Option<Box<Node>> {
    let mut tok = Tokens {
        tokens,
        idx: 0,
    };

    expr(&mut tok)
}

fn new_binary_op_node(kind: BinaryOpKind, lhs: Option<Box<Node>>, rhs: Option<Box<Node>>) -> Option<Box<Node>> {
    Some(Box::new(Node::BinaryOp{ kind, lhs, rhs }))
}

fn expr(mut tok: &mut Tokens) -> Option<Box<Node>> {
    let mut node = mul(&mut tok);

    loop {
        tok.check_illegal();
        if consume(TokenKind::Plus, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Add, node, mul(&mut tok));
        } else if consume(TokenKind::Minus, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Sub, node, mul(&mut tok));
        } else {
            return node;
        }
    }
}

fn mul(mut tok: &mut Tokens) -> Option<Box<Node>> {
    let mut node = primary(&mut tok);

    loop {
        tok.check_illegal();
        if consume(TokenKind::Asterisk, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Mul, node, primary(&mut tok));
        } else if consume(TokenKind::Slash, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Div, node, primary(&mut tok));
        } else if consume(TokenKind::Percent, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Rem, node, primary(&mut tok));
        } else {
            return node;
        }
    }
}

fn primary(tok: &mut Tokens) -> Option<Box<Node>> {
    match tok.tokens[tok.idx].kind {
        TokenKind::Integer(_) => Some(Box::new(Node::Integer(tok.expect_number()))),
        _ => None,
    }
}

fn consume(kind: TokenKind, tok: &mut Tokens) -> bool {
    if tok.tokens[tok.idx].kind == kind {
        tok.idx += 1;
        true
    } else {
        false
    }
}

impl<'a> Tokens<'a> {
    fn expect(&mut self, kind: TokenKind) {
        if self.tokens[self.idx].kind == kind {
            self.idx += 1;
        } else {
            eprintln!("{}^", " ".repeat(self.tokens[self.idx].cur));
            panic!("expected {:?}, but got {:?}", kind, self.tokens[self.idx].kind);
        }
    }

    fn expect_number(&mut self) -> u64 {
        match self.tokens[self.idx].kind {
            TokenKind::Integer(num) => {
                self.idx += 1;
                num
            },
            _ => {
                eprintln!("{}^", " ".repeat(self.tokens[self.idx].cur));
                panic!("expected Integer(u64). but got {:?}", self.tokens[self.idx].kind);
            },
        }
    }

    fn check_illegal(&self) {
        if let TokenKind::Illegal(_) = self.tokens[self.idx].kind {
            eprintln!("{}^", " ".repeat(self.tokens[self.idx].cur));
            panic!("illegal TokenKind {:?}", self.tokens[self.idx].kind);
        }
    }
}
