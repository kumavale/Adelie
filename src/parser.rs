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
// function             = 'fn' ident '(' ( param ( ',' param ) * ) ? ')' function_return_type ? block_expression
// param                = ident ':' type_no_bounds
// function_return_type = '->' type_no_bounds
//
// block_expression = '{' statement * '}'
//
// statement = 'return' ? expr ';' ?
//      | 'let' ident ':' type_no_bounds ( '=' expr ) ? ';'
//
// expr = assign
//      | 'if' expr block_expression ( 'else' block_expression ) ?
//      | 'while' expr block_expression
//      | 'loop' block_expression
//
// assign           = logical_or ( ( '=' | binary_assign_op ) expr ) ?
// binary_assign_op = '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '^=' | '|=' | '<<=' | '>>='
//
// logical_or  = logical_and ( '||' logical_and ) *
// logical_and = equality ( '&&' equality ) *
//
// equality   = relational ( '==' relational | '!=' relational ) *
// relational = bitor ( '<' bitor | '<=' bitor | '>' bitor | '>=' bitor ) *
//
// bitor  = bitxor ( '|' bitxor ) *
// bitxor = bitand ( '^' bitand ) *
// bitand = shift ( '&' shift ) *
//
// shift = add ( '<<' add | '>>' add ) *
// add   = mul ( '+' mul | '-' mul ) *
// mul   = cast ( '*' cast | '/' cast | '%' cast ) *
// cast  = unary ( 'as' type_no_bounds ) *
// unary = ( '-' | '!' | '&' | '*' ) ? primary
//
// primary = num
//         | char
//         | String
//         | bool
//         | builtin
//         | ident ( '(' ( expr ( ',' expr ) * ) ? ')' ) ?
//         | '(' expr ')'
//
// ident    = alphabet + ( num | alphabet ) *
// alphabet = 'a'..'z' | 'A'..'Z' | '_'
//
// num         = nonzero_dec dec_digit *
// dec_digit   = '0' | nonzero_dec
// nonzero_dec = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
//
// bool = 'true' | 'false'
//
// type_no_bounds = numeric | 'String' | 'bool' | 'char'
// numeric = 'i32'
//
// builtin = 'println'
//

#[derive(Debug)]
struct Parser<'a> {
    fn_symbol_table: &'a mut SymbolTable,
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
            panic!("expected `{:?}`, but got `{:?}`", kind, self.tokens[self.idx].kind);
        }
    }

    fn is_eof(&self) -> bool {
        self.tokens[self.idx].kind == TokenKind::Eof
    }
}

pub fn gen_ast<'a>(tokens: &'a [Token], fn_symbol_table: &'a mut SymbolTable) -> Vec<Function> {
    let mut parser = Parser {
        fn_symbol_table,
        current_function: None,
        tokens,
        idx: 0,
    };

    program(&mut parser)
}

fn new_binary_op_node(kind: BinaryOpKind, lhs: Node, rhs: Node) -> Node {
    Node::BinaryOp {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn new_unary_op_node(kind: UnaryOpKind, expr: Node) -> Node {
    Node::UnaryOp {
        kind,
        expr: Box::new(expr),
    }
}

fn new_assign_node(lhs: Node, rhs: Node) -> Node {
    Node::Assign {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn new_short_circuit_op_node(kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Node {
    Node::ShortCircuitOp {
        kind,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn new_if_node(cond: Node, then: Node, els: Option<Node>) -> Node {
    Node::If {
        cond: Box::new(cond),
        then: Box::new(then),
        els: els.map(Box::new),
    }
}

fn new_while_node(cond: Node, then: Node) -> Node {
    Node::While {
        cond: Box::new(cond),
        then: Box::new(then),
    }
}

fn new_loop_node(then: Node) -> Node {
    Node::Loop {
        then: Box::new(then),
    }
}

fn new_block_node(stmts: Vec<Node>) -> Node {
    Node::Block {
        stmts,
    }
}

fn new_return_node(expr: Option<Node>) -> Node {
    Node::Return {
        expr: expr.map(Box::new),
    }
}

fn new_evaluates_node(expr: Node) -> Node {
    Node::Evaluates {
        expr: Box::new(expr),
    }
}

fn new_num_node(num: i32) -> Node {
    Node::Integer {
        typekind: Type::Numeric(Numeric::I32),
        num,
    }
}

fn new_char_node(c: char) -> Node {
    Node::Integer {
        typekind: Type::Char,
        num: c as i32
    }
}

fn new_string_node(s: &str) -> Node {
    Node::String {
        typekind: Type::String,
        str: s.to_string(),
    }
}

fn new_bool_node(b: Keyword) -> Node {
    Node::Integer {
        typekind: Type::Bool,
        num: match b {
            Keyword::True  => 1,
            Keyword::False => 0,
            _ => unreachable!(),
        },
    }
}

fn new_cast_node(typekind: Type, expr: Node) -> Node {
    Node::Cast {
        typekind,
        expr: Box::new(expr),
    }
}

fn new_builtin_call_node(kind: Builtin, args: Vec<Node>) -> Node {
    Node::Builtin {
        kind,
        args,
    }
}

fn new_function_call_node(name: &str, args: Vec<Node>) -> Node {
    Node::Call {
        name: name.to_string(),
        args,
    }
}

fn new_variable_node(function: &mut Function, name: &str) -> Node {
    if let Some(obj) = function.lvar_symbol_table.find_name(name) {
        Node::Variable {
            obj: Rc::clone(obj),
        }
    } else if let Some(obj) = function.param_symbol_table.find_name(name) {
        Node::Variable {
            obj: Rc::clone(obj),
        }
    } else {
        panic!("The name '{}' does not exist in the current context", name)
    }
}

fn new_variable_node_with_let(symbol_table: &mut SymbolTable, name: &str, typekind: Type) -> Node {
    if symbol_table.find_name_current_scope(name).is_some() {
        panic!("A local variable or function named '{}' is already defined in this scope", name)
    } else {
        let obj = Rc::new(Object::new(name.to_string(), symbol_table.len(), false, typekind));
        symbol_table.push(Rc::clone(&obj));
        Node::Variable {
            obj,
        }
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
        if p.fn_symbol_table.find_name(name).is_some() {
            panic!("the name `{}` is defined multiple times", name);
        }
        let obj = Rc::new(Object::new(name.to_string(), p.fn_symbol_table.len(), false, Type::Void));
        p.fn_symbol_table.push(Rc::clone(&obj));
        p.current_function = Some(Function::new(name));
        p.idx += 1;
        p.expect(TokenKind::LParen);
        while !p.consume(TokenKind::RParen) {
            if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
                p.idx += 1;
                if p.current_function.as_mut().unwrap().param_symbol_table.find_name(name).is_some() {
                    panic!("A local variable or function named '{}' is already defined in this scope", name);
                } else {
                    p.expect(TokenKind::Colon);
                    let typekind = type_no_bounds(&mut p);
                    let current_function = p.current_function.as_mut().unwrap();
                    let obj = Rc::new(Object::new(name.to_string(), current_function.param_symbol_table.len(), true, typekind.clone()));
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

    if p.consume(TokenKind::RArrow) {
        if let TokenKind::Type(typekind) = &p.tokens[p.idx].kind {
            p.idx += 1;
            p.current_function.as_mut().unwrap().rettype = typekind.clone();
        } else {
            eprintln!("{}^", " ".repeat(p.tokens[p.idx].cur));
            panic!("expected type, but got `{:?}`", p.tokens[p.idx].kind);
        }
    }

    p.current_function.as_mut().unwrap().statements = block_expression(&mut p);
    p.current_function.take().unwrap()
}

fn statement(mut p: &mut Parser) -> Node {
    let node =  if p.consume(TokenKind::Keyword(Keyword::Return)) {
        if p.consume(TokenKind::Semi) {
            return new_return_node(None);
        }
        new_return_node(Some(expr(&mut p)))
    } else if p.consume(TokenKind::Keyword(Keyword::Let)) {
        if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
            p.idx += 1;
            p.expect(TokenKind::Colon);
            let typekind = type_no_bounds(&mut p);
            let node = new_variable_node_with_let(&mut p.current_function.as_mut().unwrap().lvar_symbol_table, name, typekind);
            if p.consume(TokenKind::Assign) {
                let node = new_assign_node(node, expr(&mut p));
                p.expect(TokenKind::Semi);
                node
            } else {
                p.expect(TokenKind::Semi);
                statement(&mut p)
            }
        } else {
            panic!("The left-hand side of an assignment must be a variable")
        }
    } else {
        expr(&mut p)
    };

    if p.consume(TokenKind::Semi) {
        node
    } else {
        new_evaluates_node(node)
    }
}

fn type_no_bounds(mut p: &mut Parser) -> Type {
    if p.consume(TokenKind::And) {
        Type::Ptr(Box::new(type_no_bounds(&mut p)))
    } else if p.consume(TokenKind::AndAnd) {
        Type::Ptr(Box::new(Type::Ptr(Box::new(type_no_bounds(&mut p)))))
    } else if let TokenKind::Type(typekind) = &p.tokens[p.idx].kind {
        p.idx += 1;
        typekind.clone()
    } else {
        panic!("expected type, but got `{:?}`", p.tokens[p.idx].kind);
    }

}

fn block_expression(mut p: &mut Parser) -> Node {
    let mut stmts = vec![];
    p.current_function.as_mut().unwrap().lvar_symbol_table.enter_scope();
    p.expect(TokenKind::LBrace );
    while !p.consume(TokenKind::RBrace) && !p.is_eof() {
        stmts.push(statement(&mut p));
    }
    p.current_function.as_mut().unwrap().lvar_symbol_table.leave_scope();
    new_block_node(stmts)
}

fn expr(mut p: &mut Parser) -> Node {
    if p.tokens[p.idx].kind == TokenKind::LBrace {
        block_expression(&mut p)
    } else if p.consume(TokenKind::Keyword(Keyword::If)) {
        let cond = expr(&mut p);
        let then = block_expression(&mut p);
        let els = if p.consume(TokenKind::Keyword(Keyword::Else)) {
            Some(block_expression(&mut p))
        } else {
            None
        };
        new_if_node(cond, then, els)
    } else if p.consume(TokenKind::Keyword(Keyword::While)) {
        let cond = expr(&mut p);
        let then = block_expression(&mut p);
        new_while_node(cond, then)
    } else if p.consume(TokenKind::Keyword(Keyword::Loop)) {
        new_loop_node(block_expression(&mut p))
    } else {
        assign(&mut p)
    }
}

fn assign(mut p: &mut Parser) -> Node {
    let node = logical_or(&mut p);

    if p.consume(TokenKind::Assign) {
        new_assign_node(node, expr(&mut p))
    } else if p.consume(TokenKind::PlusEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Add, node, expr(&mut p)))
    } else if p.consume(TokenKind::MinusEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Sub, node, expr(&mut p)))
    } else if p.consume(TokenKind::StarEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Mul, node, expr(&mut p)))
    } else if p.consume(TokenKind::SlashEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Div, node, expr(&mut p)))
    } else if p.consume(TokenKind::PercentEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Rem, node, expr(&mut p)))
    } else if p.consume(TokenKind::AndEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitAnd, node, expr(&mut p)))
    } else if p.consume(TokenKind::CaretEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitXor, node, expr(&mut p)))
    } else if p.consume(TokenKind::OrEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitOr, node, expr(&mut p)))
    } else if p.consume(TokenKind::ShlEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Shl, node, expr(&mut p)))
    } else if p.consume(TokenKind::ShrEq) {
        let lhs = node.clone();
        new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Shr, node, expr(&mut p)))
    } else {
        node
    }
}

fn logical_or(mut p: &mut Parser) -> Node {
    let mut node = logical_and(&mut p);

    loop {
        if p.consume(TokenKind::OrOr) {
            node = new_short_circuit_op_node(ShortCircuitOpKind::Or, node, logical_and(&mut p));
        } else {
            return node;
        }
    }
}

fn logical_and(mut p: &mut Parser) -> Node {
    let mut node = equality(&mut p);

    loop {
        if p.consume(TokenKind::AndAnd) {
            node = new_short_circuit_op_node(ShortCircuitOpKind::And, node, equality(&mut p));
        } else {
            return node;
        }
    }
}

fn equality(mut p: &mut Parser) -> Node {
    let mut node = relational(&mut p);

    loop {
        if p.consume(TokenKind::EqEq) {
            node = new_binary_op_node(BinaryOpKind::Eq, node, relational(&mut p));
        } else if p.consume(TokenKind::Ne) {
            node = new_binary_op_node(BinaryOpKind::Ne, node, relational(&mut p));
        } else {
            return node;
        }
    }
}

fn relational(mut p: &mut Parser) -> Node {
    let mut node = bitor(&mut p);

    loop {
        if p.consume(TokenKind::Lt) {
            node = new_binary_op_node(BinaryOpKind::Lt, node, bitor(&mut p));
        } else if p.consume(TokenKind::Le) {
            node = new_binary_op_node(BinaryOpKind::Le, node, bitor(&mut p));
        } else if p.consume(TokenKind::Gt) {
            node = new_binary_op_node(BinaryOpKind::Gt, node, bitor(&mut p));
        } else if p.consume(TokenKind::Ge) {
            node = new_binary_op_node(BinaryOpKind::Ge, node, bitor(&mut p));
        } else {
            return node;
        }
    }
}

fn bitor(mut p: &mut Parser) -> Node {
    let mut node = bitxor(&mut p);
    while p.consume(TokenKind::Or) {
        node = new_binary_op_node(BinaryOpKind::BitOr, node, bitxor(&mut p));
    }
    node
}

fn bitxor(mut p: &mut Parser) -> Node {
    let mut node = bitand(&mut p);
    while p.consume(TokenKind::Caret) {
        node = new_binary_op_node(BinaryOpKind::BitXor, node, bitand(&mut p));
    }
    node
}

fn bitand(mut p: &mut Parser) -> Node {
    let mut node = shift(&mut p);
    while p.consume(TokenKind::And) {
        node = new_binary_op_node(BinaryOpKind::BitAnd, node, shift(&mut p));
    }
    node
}

fn shift(mut p: &mut Parser) -> Node {
    let mut node = add(&mut p);

    loop {
        if p.consume(TokenKind::Shl) {
            node = new_binary_op_node(BinaryOpKind::Shl, node, add(&mut p));
        } else if p.consume(TokenKind::Shr) {
            node = new_binary_op_node(BinaryOpKind::Shr, node, add(&mut p));
        } else {
            return node;
        }
    }
}

fn add(mut p: &mut Parser) -> Node {
    let mut node = mul(&mut p);

    loop {
        if p.consume(TokenKind::Plus) {
            node = new_binary_op_node(BinaryOpKind::Add, node, mul(&mut p));
        } else if p.consume(TokenKind::Minus) {
            node = new_binary_op_node(BinaryOpKind::Sub, node, mul(&mut p));
        } else {
            return node;
        }
    }
}

fn mul(mut p: &mut Parser) -> Node {
    let mut node = cast(&mut p);

    loop {
        if p.consume(TokenKind::Star) {
            node = new_binary_op_node(BinaryOpKind::Mul, node, cast(&mut p));
        } else if p.consume(TokenKind::Slash) {
            node = new_binary_op_node(BinaryOpKind::Div, node, cast(&mut p));
        } else if p.consume(TokenKind::Percent) {
            node = new_binary_op_node(BinaryOpKind::Rem, node, cast(&mut p));
        } else {
            return node;
        }
    }
}

fn cast(mut p: &mut Parser) -> Node {
    let mut node = unary(&mut p);

    loop {
        if p.consume(TokenKind::Keyword(Keyword::As)) {
            let typekind = type_no_bounds(&mut p);
            node = new_cast_node(typekind, node);
        } else {
            return node;
        }
    }
}

fn unary(mut p: &mut Parser) -> Node {
    if p.consume(TokenKind::Minus) {
        new_unary_op_node(UnaryOpKind::Neg, unary(&mut p))
    } else if p.consume(TokenKind::Not) {
        new_unary_op_node(UnaryOpKind::Not, unary(&mut p))
    } else if p.consume(TokenKind::And) {
        new_unary_op_node(UnaryOpKind::Ref, unary(&mut p))
    } else if p.consume(TokenKind::Star) {
        new_unary_op_node(UnaryOpKind::Deref, unary(&mut p))
    } else {
        primary(&mut p)
    }
}

fn primary(mut p: &mut Parser) -> Node {
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
        TokenKind::Char(c) => {
            p.idx += 1;
            new_char_node(*c)
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
                    args.push(expr(&mut p));
                    p.consume(TokenKind::Comma);
                }
                new_function_call_node(name, args)
            } else {
                // local variable or parameter
                new_variable_node(&mut p.current_function.as_mut().unwrap(), name)
            }
        }
        TokenKind::Keyword(b) if matches!(b, Keyword::True|Keyword::False) => {
            p.idx += 1;
            new_bool_node(*b)
        }
        TokenKind::Builtin(kind) => {
            p.idx += 1;
            p.expect(TokenKind::LParen);
            let mut args = vec![];
            while !p.consume(TokenKind::RParen) {
                args.push(expr(&mut p));
                p.consume(TokenKind::Comma);
            }
            new_builtin_call_node(*kind, args)
        }
        _ => {
            eprintln!("{}^", " ".repeat(p.tokens[p.idx].cur));
            panic!("illegal TokenKind `{:?}`", p.tokens[p.idx].kind);
        }
    }
}
