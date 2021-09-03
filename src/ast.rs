use super::token::*;

// EBNF
//
// program = stmt *
// stmt    = 'return' ? expr ';'
// expr    = assign
//
// assign     = equality ( ( '=' | BinaryAssignOp ) assign ) ?
// equality   = relational ( '==' relational | '!=' relational ) *
// relational = add ( '<' add | '<=' add | '>' add | '>=' add ) *
//
// add     = mul ( '+' mul | '-' mul ) *
// mul     = unary ( '*' unary | '/' unary | '%' unary ) *
// unary   = ( '-' ) ? primary
//
// primary = num | ident | '(' expr ')'
//
// ident = 'a'..'z' | 'A'..'Z'
//
// num         = nonzero_dec dec_digit *
// dec_digit   = '0' | nonzero_dec
// nonzero_dec = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
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
    Eq,   // ==
    Lt,   // <
    Le,   // <=
    Ne,   // !=
    Gt,   // >
    Ge,   // >=
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssignOpKind {
    Assign,  // =
    BinaryAssignOpKind,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryAssignOpKind {
    Add,  // +=
    Sub,  // -=
    Mul,  // *=
    Div,  // /=
    Rem,  // %=
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Integer(u64),  // [1-9][0-9]*
    Variable {
        name: String,  // [a-zA-Z]
        offset: usize,
    },
    Assign {
        kind: AssignOpKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Return {
        expr: Box<Node>,
    },
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

pub fn gen_ast(tokens: &[Token]) -> Vec<Box<Node>> {
    let mut tok = Tokens {
        tokens,
        idx: 0,
    };

    program(&mut tok)
}

fn new_binary_op_node(kind: BinaryOpKind, lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::BinaryOp{ kind, lhs, rhs })
}

fn new_unary_op_node(kind: UnaryOpKind, expr: Box<Node>) -> Box<Node> {
    Box::new(Node::UnaryOp { kind, expr })
}

fn new_variable_node(name: &str) -> Box<Node> {
    let offset = 0;
    Box::new(Node::Variable { name: name.to_string(), offset })
}

fn new_return_node(expr: Box<Node>) -> Box<Node> {
    Box::new(Node::Return { expr })
}

fn program(mut tok: &mut Tokens) -> Vec<Box<Node>> {
    let mut code = vec![];
    while !tok.is_eof() {
        code.push(stmt(&mut tok));
    }
    code
}

fn stmt(mut tok: &mut Tokens) -> Box<Node> {
    let node = if tok.consume(TokenKind::Keyword(Keywords::Return)) {
        new_return_node(expr(&mut tok))
    } else {
        expr(&mut tok)
    };

    if tok.consume(TokenKind::Semicolon) {
        return node
    } else {
        todo!("implicit return")
    }
}

fn expr(mut tok: &mut Tokens) -> Box<Node> {
    equality(&mut tok)
}

fn equality(mut tok: &mut Tokens) -> Box<Node> {
    let mut node = relational(&mut tok);

    loop {
        tok.check_illegal();
        if tok.consume(TokenKind::Eq) {
            node = new_binary_op_node(BinaryOpKind::Eq, node, relational(&mut tok));
        } else if tok.consume(TokenKind::Ne) {
            node = new_binary_op_node(BinaryOpKind::Ne, node, relational(&mut tok));
        } else {
            return node;
        }
    }
}

fn relational(mut tok: &mut Tokens) -> Box<Node> {
    let mut node = add(&mut tok);

    loop {
        tok.check_illegal();
        if tok.consume(TokenKind::Lt) {
            node = new_binary_op_node(BinaryOpKind::Lt, node, add(&mut tok));
        } else if tok.consume(TokenKind::Le) {
            node = new_binary_op_node(BinaryOpKind::Le, node, add(&mut tok));
        } else if tok.consume(TokenKind::Gt) {
            node = new_binary_op_node(BinaryOpKind::Gt, node, add(&mut tok));
        } else if tok.consume(TokenKind::Ge) {
            node = new_binary_op_node(BinaryOpKind::Ge, node, add(&mut tok));
        } else {
            return node;
        }
    }
}

fn add(mut tok: &mut Tokens) -> Box<Node> {
    let mut node = mul(&mut tok);

    loop {
        tok.check_illegal();
        if tok.consume(TokenKind::Plus) {
            node = new_binary_op_node(BinaryOpKind::Add, node, mul(&mut tok));
        } else if tok.consume(TokenKind::Minus) {
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
        if tok.consume(TokenKind::Asterisk) {
            node = new_binary_op_node(BinaryOpKind::Mul, node, unary(&mut tok));
        } else if tok.consume(TokenKind::Slash) {
            node = new_binary_op_node(BinaryOpKind::Div, node, unary(&mut tok));
        } else if tok.consume(TokenKind::Percent) {
            node = new_binary_op_node(BinaryOpKind::Rem, node, unary(&mut tok));
        } else {
            return node;
        }
    }
}

fn unary(mut tok: &mut Tokens) -> Box<Node> {
    if tok.consume(TokenKind::Minus) {
        new_unary_op_node(UnaryOpKind::Neg, unary(&mut tok))
    } else {
        primary(&mut tok)
    }
}

fn primary(mut tok: &mut Tokens) -> Box<Node> {
    if tok.consume(TokenKind::LParen) {
        let node = expr(&mut tok);
        tok.expect(TokenKind::RParen);
        return node;
    }

    match &tok.tokens[tok.idx].kind {
        TokenKind::Integer(num) => {
            tok.idx += 1;
            Box::new(Node::Integer(*num))
        }
        TokenKind::Ident(name) => {
            new_variable_node(&name)
        }
        _ => {
            eprintln!("{}^", " ".repeat(tok.tokens[tok.idx].cur));
            panic!("illegal TokenKind {:?}", tok.tokens[tok.idx].kind);
        }
    }
}

impl<'a> Tokens<'a> {
    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.tokens[self.idx].kind == kind {
            self.idx += 1;
            true
        } else {
            false
        }
    }

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

    fn is_eof(&self) -> bool {
        self.tokens[self.idx].kind == TokenKind::Eof
    }
}
