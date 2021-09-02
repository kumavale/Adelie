use super::token::*;

// EBNF
//
// expr    = mul ( '+' mul | '-' mul ) * ;
// mul     = unary ( '*' unary | '/' unary | '%' unary ) * ;
// unary   = ( '-' ) ? primary ;
// primary = nonzero_dec dec_digit * ;
//
// dec_digit   = '0' | nonzero_dec ;
// nonzero_dec = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
//

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOpKind {
    Neg,  // -
}

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
    UnaryOp {
        kind: UnaryOpKind,
        expr: Box<Node>,
    },
    BinaryOp {
        kind: BinaryOpKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}

#[derive(Debug)]
struct Tokens<'a> {
    tokens: &'a [Token],
    idx: usize,
}

pub fn gen_ast(tokens: &[Token]) -> Box<Node> {
    let mut tok = Tokens {
        tokens,
        idx: 0,
    };

    expr(&mut tok)
}

fn new_binary_op_node(kind: BinaryOpKind, lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::BinaryOp{ kind, lhs, rhs })
}

fn new_unary_op_node(kind: UnaryOpKind, expr: Box<Node>) -> Box<Node> {
    Box::new(Node::UnaryOp { kind, expr })
}

fn expr(mut tok: &mut Tokens) ->Box<Node> {
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

fn mul(mut tok: &mut Tokens) -> Box<Node> {
    let mut node = unary(&mut tok);

    loop {
        tok.check_illegal();
        if consume(TokenKind::Asterisk, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Mul, node, unary(&mut tok));
        } else if consume(TokenKind::Slash, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Div, node, unary(&mut tok));
        } else if consume(TokenKind::Percent, &mut tok) {
            node = new_binary_op_node(BinaryOpKind::Rem, node, unary(&mut tok));
        } else {
            return node;
        }
    }
}

fn unary(mut tok: &mut Tokens) ->Box<Node> {
    if consume(TokenKind::Minus, &mut tok) {
        new_unary_op_node(UnaryOpKind::Neg, unary(&mut tok))
    } else {
        primary(&mut tok)
    }
}

fn primary(mut tok: &mut Tokens) ->Box<Node> {
    if consume(TokenKind::LParen, &mut tok) {
        let node = expr(&mut tok);
        tok.expect(TokenKind::RParen);
        return node;
    }

    match tok.tokens[tok.idx].kind {
        TokenKind::Integer(num) => {
            tok.idx += 1;
            Box::new(Node::Integer(num))
        }
        _ => {
            eprintln!("{}^", " ".repeat(tok.tokens[tok.idx].cur));
            panic!("illegal TokenKind {:?}", tok.tokens[tok.idx].kind);
        }
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

    fn check_illegal(&self) {
        if let TokenKind::Illegal(_) = self.tokens[self.idx].kind {
            eprintln!("{}^", " ".repeat(self.tokens[self.idx].cur));
            panic!("illegal TokenKind {:?}", self.tokens[self.idx].kind);
        }
    }
}
