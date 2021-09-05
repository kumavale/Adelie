use std::rc::Rc;
use super::ast::*;
use super::token::*;
use super::keyword::*;
use super::object::*;
use super::function::*;
use super::builtin::*;

// EBNF
//
// program = function *
//
// function = 'fn' ident '(' ( param ( ',' param ) * ) ? ')' compound_stmt
// param    = ident ':' type
//
// compound_stmt = '{' stmt * '}'
//
// stmt = 'return' ? expr ';'
//      | 'let' ident ':' type ( '=' expr ) ? ';'
//
// expr = assign
//      | 'if' expr compound_stmt ( 'else' compound_stmt ) ?
//      | 'while' expr compound_stmt
//
// assign           = equality ( ( '=' | binary_assign_op ) assign ) ?
// binary_assign_op = '+=' | '-=' | '*=' | '/=' | '%='
// equality         = relational ( '==' relational | '!=' relational ) *
// relational       = add ( '<' add | '<=' add | '>' add | '>=' add ) *
//
// add   = mul ( '+' mul | '-' mul ) *
// mul   = unary ( '*' unary | '/' unary | '%' unary ) *
// unary = ( '-' ) ? primary
//
// primary = num
//         | builtin
//         | ident ( '(' ( assign ( ',' assign ) * ) ? ')' ) ?
//         | '(' expr ')'
//
// ident    = alphabet + ( num | alphabet ) *
// alphabet = 'a'..'z' | 'A'..'Z' | '_'
//
// num         = nonzero_dec dec_digit *
// dec_digit   = '0' | nonzero_dec
// nonzero_dec = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
//
// type = 'i32' | 'String'
//
// builtin = 'println'
//

#[derive(Debug)]
struct Parser<'a> {
    symbol_table: &'a mut SymbolTable,
    current_function: Option<Function>,
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

pub fn gen_ast<'a>(tokens: &'a [Token], symbol_table: &'a mut SymbolTable) -> Vec<Function> {
    let mut parser = Parser {
        symbol_table,
        current_function: None,
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

fn new_if_node(cond: Box<Node>, then: Box<Node>, els: Option<Box<Node>>) -> Box<Node> {
    Box::new(Node::If { cond, then, els })
}

fn new_while_node(cond: Box<Node>, then: Box<Node>) -> Box<Node> {
    Box::new(Node::While { cond, then })
}

fn new_block_node(stmts: Vec<Box<Node>>) -> Box<Node> {
    Box::new(Node::Block { stmts })
}

fn new_pop_node(expr: Box<Node>) -> Box<Node> {
    Box::new(Node::Pop { expr })
}

fn new_return_node(expr: Box<Node>) -> Box<Node> {
    Box::new(Node::Return { expr })
}

fn new_num_node(num: i32) -> Box<Node> {
    Box::new(Node::Integer(num))
}

fn new_string_node(s: &str) -> Box<Node> {
    Box::new(Node::String(s.to_string()))
}

fn new_builtin_call_node(kind: Builtin, args: Vec<Box<Node>>) -> Box<Node> {
    Box::new(Node::Builtin { kind, args })
}

fn new_function_call_node(function: &mut Function, name: &str, args: Vec<Box<Node>>) -> Box<Node> {
    if let Some(obj) = function.lvar_symbol_table.find_lvar(name) {
        Box::new(Node::Function {
            obj: Rc::clone(obj),
            args,
        })
    } else {
        //todo!("use");
        //panic!("The name '{}' does not exist in the current context", name);
        Box::new(Node::Function {
            obj: Rc::new(Object::new(name.to_string(), 0, false, Type::Unknown)),
            args,
        })
    }
}

fn new_variable_node(function: &mut Function, name: &str) -> Box<Node> {
    if let Some(obj) = function.lvar_symbol_table.find_lvar(name) {
        Box::new(Node::Variable(Rc::clone(obj)))
    } else if let Some(obj) = function.param_symbol_table.find_lvar(name) {
        Box::new(Node::Variable(Rc::clone(obj)))
    } else {
        panic!("The name '{}' does not exist in the current context", name)
    }
}

fn new_variable_node_with_let(symbol_table: &mut SymbolTable, name: &str, typekind: Type) -> Box<Node> {
    if symbol_table.find_lvar(name).is_some() {
        panic!("A local variable or function named '{}' is already defined in this scope", name)
    } else {
        let obj = Rc::new(Object::new(name.to_string(), symbol_table.len(), false, typekind));
        symbol_table.push(Rc::clone(&obj));
        Box::new(Node::Variable(obj))
    }
}

fn program(mut p: &mut Parser) -> Vec<Function> {
    let mut functions = vec![];
    while !p.is_eof() {
        functions.push(function(&mut p));
    }
    functions
}

fn function(mut p: &mut Parser) -> Function {
    p.expect(TokenKind::Keyword(Keyword::Fn));
    if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
        if p.symbol_table.find_lvar(name).is_some() {
            panic!("The name '{}' does not exist in the current context", name);
        }
        let obj = Rc::new(Object::new(name.to_string(), p.symbol_table.len(), false, Type::Numeric(Numeric::I32)));
        p.symbol_table.push(Rc::clone(&obj));
        p.current_function = Some(Function::new(name));
        p.idx += 1;
        p.expect(TokenKind::LParen);
        while !p.consume(TokenKind::RParen) {
            if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
                p.idx += 1;
                if p.current_function.as_mut().unwrap().param_symbol_table.find_lvar(name).is_some() {
                    panic!("A local variable or function named '{}' is already defined in this scope", name)
                } else {
                    p.expect(TokenKind::Colon);
                    let typekind = if let TokenKind::Type(typekind) = p.tokens[p.idx].kind {
                        typekind
                    } else {
                        eprintln!("{}^", " ".repeat(p.tokens[p.idx].cur));
                        panic!("expected type, but got {:?}", p.tokens[p.idx].kind);
                    };
                    p.idx += 1;
                    let current_function = p.current_function.as_mut().unwrap();
                    let obj = Rc::new(Object::new(name.to_string(), current_function.param_symbol_table.len(), true, typekind));
                    current_function.param_symbol_table.push(Rc::clone(&obj));
                }
            } else {
                panic!("Identifier expected");
            }
            p.consume(TokenKind::Comma);
        }
    } else {
        eprintln!("{}^", " ".repeat(p.tokens[p.idx].cur));
        panic!("expected identifier");
    }

    let statements = compound_stmt(&mut p);

    p.current_function.as_mut().unwrap().statements = match *statements {
        Node::Block { ref stmts } if !stmts.is_empty() => Some(*statements),
        _ => None
    };

    p.current_function.take().unwrap()
}

fn stmt(mut p: &mut Parser) -> Box<Node> {
    let node = if p.tokens[p.idx].kind == TokenKind::LBlock {
        compound_stmt(&mut p)
    } else if p.consume(TokenKind::Keyword(Keyword::Return)) {
        new_return_node(expr(&mut p))
    } else if p.consume(TokenKind::Keyword(Keyword::Let)) {
        if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
            p.idx += 1;
            p.expect(TokenKind::Colon);
            let mut node = if let TokenKind::Type(typekind) = p.tokens[p.idx].kind {
                p.idx += 1;
                new_variable_node_with_let(&mut p.current_function.as_mut().unwrap().lvar_symbol_table, name, typekind)
            } else {
                eprintln!("{}^", " ".repeat(p.tokens[p.idx].cur));
                panic!("expected type, but got {:?}", p.tokens[p.idx].kind);
            };
            if p.consume(TokenKind::Assign) {
                node = new_assign_node(node, expr(&mut p))
            };
            node
        } else {
            panic!("The left-hand side of an assignment must be a variable")
        }
    } else {
        expr(&mut p)
    };

    if p.consume(TokenKind::Semicolon) {
        new_pop_node(node)
    } else {
        node
        //todo!("implicit return")
    }
}

fn compound_stmt(mut p: &mut Parser) -> Box<Node> {
    let mut block = new_block_node(vec![]);
    p.expect(TokenKind::LBlock);
    while !p.consume(TokenKind::RBlock) && !p.is_eof() {
        if let Node::Block{ ref mut stmts } = *block {
            stmts.push(stmt(&mut p));
        }
    }
    block
}

fn expr(mut p: &mut Parser) -> Box<Node> {
    if p.consume(TokenKind::Keyword(Keyword::If)) {
        let cond = expr(&mut p);
        let then = compound_stmt(&mut p);
        let els = if p.consume(TokenKind::Keyword(Keyword::Else)) {
            Some(compound_stmt(&mut p))
        } else {
            None
        };
        new_if_node(cond, then, els)
    } else if p.consume(TokenKind::Keyword(Keyword::While)) {
        let cond = expr(&mut p);
        let then = compound_stmt(&mut p);
        new_while_node(cond, then)
    } else {
        assign(&mut p)
    }
}

fn assign(mut p: &mut Parser) -> Box<Node> {
    let node = equality(&mut p);

    if p.consume(TokenKind::Assign) {
        new_assign_node(node, assign(&mut p))
    } else if p.consume(TokenKind::AddAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Add, node, expr(&mut p)))
    } else if p.consume(TokenKind::SubAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Sub, node, expr(&mut p)))
    } else if p.consume(TokenKind::MulAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Mul, node, expr(&mut p)))
    } else if p.consume(TokenKind::DivAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Div, node, expr(&mut p)))
    } else if p.consume(TokenKind::RemAssign) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Rem, node, expr(&mut p)))
    } else {
        node
    }
}

fn equality(mut p: &mut Parser) -> Box<Node> {
    let mut node = relational(&mut p);

    loop {
        p.check_illegal();
        if p.consume(TokenKind::Eq) {
            node = new_binary_op_node(BinaryOpKind::Eq, node, relational(&mut p));
        } else if p.consume(TokenKind::Ne) {
            node = new_binary_op_node(BinaryOpKind::Ne, node, relational(&mut p));
        } else {
            return node;
        }
    }
}

fn relational(mut p: &mut Parser) -> Box<Node> {
    let mut node = add(&mut p);

    loop {
        p.check_illegal();
        if p.consume(TokenKind::Lt) {
            node = new_binary_op_node(BinaryOpKind::Lt, node, add(&mut p));
        } else if p.consume(TokenKind::Le) {
            node = new_binary_op_node(BinaryOpKind::Le, node, add(&mut p));
        } else if p.consume(TokenKind::Gt) {
            node = new_binary_op_node(BinaryOpKind::Gt, node, add(&mut p));
        } else if p.consume(TokenKind::Ge) {
            node = new_binary_op_node(BinaryOpKind::Ge, node, add(&mut p));
        } else {
            return node;
        }
    }
}

fn add(mut p: &mut Parser) -> Box<Node> {
    let mut node = mul(&mut p);

    loop {
        p.check_illegal();
        if p.consume(TokenKind::Plus) {
            node = new_binary_op_node(BinaryOpKind::Add, node, mul(&mut p));
        } else if p.consume(TokenKind::Minus) {
            node = new_binary_op_node(BinaryOpKind::Sub, node, mul(&mut p));
        } else {
            return node;
        }
    }
}

fn mul(mut p: &mut Parser) -> Box<Node> {
    let mut node = unary(&mut p);

    loop {
        p.check_illegal();
        if p.consume(TokenKind::Asterisk) {
            node = new_binary_op_node(BinaryOpKind::Mul, node, unary(&mut p));
        } else if p.consume(TokenKind::Slash) {
            node = new_binary_op_node(BinaryOpKind::Div, node, unary(&mut p));
        } else if p.consume(TokenKind::Percent) {
            node = new_binary_op_node(BinaryOpKind::Rem, node, unary(&mut p));
        } else {
            return node;
        }
    }
}

fn unary(mut p: &mut Parser) -> Box<Node> {
    if p.consume(TokenKind::Minus) {
        new_unary_op_node(UnaryOpKind::Neg, unary(&mut p))
    } else {
        primary(&mut p)
    }
}

fn primary(mut p: &mut Parser) -> Box<Node> {
    if p.consume(TokenKind::LParen) {
        let node = expr(&mut p);
        p.expect(TokenKind::RParen);
        return node;
    }

    match &p.tokens[p.idx].kind {
        TokenKind::Integer(num) => {
            p.idx += 1;
            new_num_node(*num)
        }
        TokenKind::String(s) => {
            p.idx += 1;
            new_string_node(s)
        }
        TokenKind::Ident(name) => {
            p.idx += 1;
            if p.consume(TokenKind::LParen) {
                // function
                let mut args = vec![];
                while !p.consume(TokenKind::RParen) {
                    args.push(assign(&mut p));
                    p.consume(TokenKind::Comma);
                }
                new_function_call_node(&mut p.current_function.as_mut().unwrap(), name, args)
            } else {
                // local variable or parameter
                new_variable_node(&mut p.current_function.as_mut().unwrap(), name)
            }
        }
        TokenKind::Builtin(kind) => {
            p.idx += 1;
            p.expect(TokenKind::LParen);
            let mut args = vec![];
            while !p.consume(TokenKind::RParen) {
                args.push(assign(&mut p));
                p.consume(TokenKind::Comma);
            }
            new_builtin_call_node(*kind, args)
        }
        _ => {
            eprintln!("{}^", " ".repeat(p.tokens[p.idx].cur));
            panic!("illegal TokenKind {:?}", p.tokens[p.idx].kind);
        }
    }
}
