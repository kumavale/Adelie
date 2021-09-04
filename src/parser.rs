use std::rc::Rc;
use super::ast::*;
use super::token::*;
use super::object::*;

// EBNF
//
// program = stmt *
// stmt    = 'return' ? expr ';'
//         | 'let' ident ( '=' expr ) ? ';'
// expr    = assign
//
// assign           = equality ( ( '=' | binary_assign_op ) assign ) ?
// binary_assign_op = '+=' | '-=' | '*=' | '/=' | '%='
// equality         = relational ( '==' relational | '!=' relational ) *
// relational       = add ( '<' add | '<=' add | '>' add | '>=' add ) *
//
// add     = mul ( '+' mul | '-' mul ) *
// mul     = unary ( '*' unary | '/' unary | '%' unary ) *
// unary   = ( '-' ) ? primary
//
// primary = num | ident | '(' expr ')'
//
// ident    = alphabet + ( num | alphabet ) *
// alphabet = 'a'..'z' | 'A'..'Z' | '_'
//
// num         = nonzero_dec dec_digit *
// dec_digit   = '0' | nonzero_dec
// nonzero_dec = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
//

#[derive(Debug)]
struct Parser<'a> {
    symbol_table: &'a mut SymbolTable,
    tokens: &'a [Token],
    idx: usize,
}

impl<'a> Parser<'a> {
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

pub fn gen_ast<'a>(tokens: &'a [Token], symbol_table: &'a mut SymbolTable) -> Vec<Box<Node>> {
    let mut parser = Parser {
        symbol_table,
        tokens,
        idx: 0,
    };

    program(&mut parser)
}

fn new_binary_op_node(kind: BinaryOpKind, lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::BinaryOp{ kind, lhs, rhs })
}

fn new_unary_op_node(kind: UnaryOpKind, expr: Box<Node>) -> Box<Node> {
    Box::new(Node::UnaryOp { kind, expr })
}

fn new_assign_node(lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::Assign { lhs, rhs })
}

fn new_num_node(num: i32) -> Box<Node> {
    Box::new(Node::Integer(num))
}

fn new_variable_node(symbol_table: &mut SymbolTable, name: &str) -> Box<Node> {
    if let Some(obj) = symbol_table.find_lvar(name) {
        Box::new(Node::Variable(Rc::clone(obj)))
    } else {
        panic!("The name '{}' does not exist in the current context", name)
    }
}

fn new_variable_node_with_let(symbol_table: &mut SymbolTable, name: &str) -> Box<Node> {
    if symbol_table.find_lvar(name).is_some() {
        panic!("A local variable or function named '{}' is already defined in this scope", name)
    } else {
        let obj = Rc::new(Object::new(name.to_string(), symbol_table.len()));
        symbol_table.push(Rc::clone(&obj));
        Box::new(Node::Variable(obj))
    }
}

fn new_return_node(expr: Box<Node>) -> Box<Node> {
    Box::new(Node::Return { expr })
}

fn program(mut tok: &mut Parser) -> Vec<Box<Node>> {
    let mut code = vec![];
    while !tok.is_eof() {
        code.push(stmt(&mut tok));
    }
    code
}

fn stmt(mut tok: &mut Parser) -> Box<Node> {
    let node = if tok.consume(TokenKind::Keyword(Keywords::Return)) {
        new_return_node(expr(&mut tok))
    } else if tok.consume(TokenKind::Keyword(Keywords::Let)) {
        if let TokenKind::Ident(name) = &tok.tokens[tok.idx].kind {
            tok.idx += 1;
            let mut node = new_variable_node_with_let(&mut tok.symbol_table, name);
            if tok.consume(TokenKind::Assign) {
                node = new_assign_node(node, expr(&mut tok))
            }
            node
        } else {
            panic!("The left-hand side of an assignment must be a variable")
        }
    } else {
        expr(&mut tok)
    };

    if tok.consume(TokenKind::Semicolon) {
        node
    } else {
        todo!("implicit return")
    }
}

fn expr(mut tok: &mut Parser) -> Box<Node> {
    assign(&mut tok)
}

fn assign(mut tok: &mut Parser) -> Box<Node> {
    let node = equality(&mut tok);

    if tok.consume(TokenKind::Assign) {
        new_assign_node(node, assign(&mut tok))
    } else if tok.consume(TokenKind::AddAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Add, node, expr(&mut tok)))
    } else if tok.consume(TokenKind::SubAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Sub, node, expr(&mut tok)))
    } else if tok.consume(TokenKind::MulAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Mul, node, expr(&mut tok)))
    } else if tok.consume(TokenKind::DivAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Div, node, expr(&mut tok)))
    } else if tok.consume(TokenKind::RemAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Rem, node, expr(&mut tok)))
    } else {
        node
    }
}

fn equality(mut tok: &mut Parser) -> Box<Node> {
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

fn relational(mut tok: &mut Parser) -> Box<Node> {
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

fn add(mut tok: &mut Parser) -> Box<Node> {
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

fn mul(mut tok: &mut Parser) -> Box<Node> {
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

fn unary(mut tok: &mut Parser) -> Box<Node> {
    if tok.consume(TokenKind::Minus) {
        new_unary_op_node(UnaryOpKind::Neg, unary(&mut tok))
    } else {
        primary(&mut tok)
    }
}

fn primary(mut tok: &mut Parser) -> Box<Node> {
    if tok.consume(TokenKind::LParen) {
        let node = expr(&mut tok);
        tok.expect(TokenKind::RParen);
        return node;
    }

    match &tok.tokens[tok.idx].kind {
        TokenKind::Integer(num) => {
            tok.idx += 1;
            new_num_node(*num)
        }
        TokenKind::Ident(name) => {
            tok.idx += 1;
            new_variable_node(&mut tok.symbol_table, name)
        }
        _ => {
            eprintln!("{}^", " ".repeat(tok.tokens[tok.idx].cur));
            panic!("illegal TokenKind {:?}", tok.tokens[tok.idx].kind);
        }
    }
}
