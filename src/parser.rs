use std::rc::Rc;
use super::ast::*;
use super::token::*;
use super::keyword::*;
use super::object::*;
use super::function::*;
use super::builtin::*;
use super::program::*;
use super::class::*;

// Grammar
//
// Program = Item *
//
// Statement :
//     `;`
//   | Item
//   | LetStatement
//   | ExpressionStatement
//
// Item :
//     Function
//   | Struct
//   | Implementation
//
// Function :
//     `fn` IDENTIFIER `(` FunctionParameters ? `)` FunctionReturnType ? BlockExpression
// FunctionParameters :
//     FunctionParam ( `,` FunctionParam ) * `,` ?
// FunctionParam :
//     IDENTIFIER `:` Type
// FunctionReturnType :
//     `->` Type
//
// Struct :
//     `struct` IDENTIFIER `{` StructFields ? `}`
// StructFields :
//     StructField ( `,` StructField ) * `,` ?
// StructField :
//     IDENTIFIER `:` Type
//
// Implementation :
//     `impl` Type `{` AssociatedItem * `}`
// AssociatedItem :
//     Function
//
// LetStatement :
//     `let` IDENTIFIER `:` Type ( `=` Expression ) ? `;`
//
// ExpressionStatement :
//     ExpressionWithoutBlock `;`
//   | ExpressionWithBlock `;` ?
//
// Expression = ExpressionWithoutBlock | ExpressionWithBlock
// ExpressionWithoutBlock :
//     LiteralExpression
//   | OperatorExpression
//   | GroupedExpression
//   | StructExpression
//   | CallExpression
//   | FieldExpression
//   | ReturnExpression
// ExpressionWithBlock :
//     BlockExpression
//   | LoopExpression
//   | IfExpression
//
// LiteralExpression :
//     CHAR_LITERAL
//   | STRING_LITERAL
//   | INTEGER_LITERAL
//   | BOOLEAN_LITERAL
//
// OperatorExpression :
//     BorrowExpression
//   | DereferenceExpression
//   | NegationExpression
//   | ArithmeticOrLogicalExpression
//   | ComparisonExpression
//   | LazyBooleanExpression
//   | TypeCastExpression
//   | AssignmentExpression
//   | CompoundAssignmentExpression
//
// GroupedExpression :
//     `(` Expression `)`
//
// StructExpression :
//     StructExprStruct
// StructExprStruct :
//     IDENTIFIER `{` StructExprFields `}`
// StructExprFields :
//     StructExprField ( `,` StructExprField ) * `,` ?
// StructExprField :
//     IDENTIFIER
//
// CallExpression :
//     Expression `(` CallParams ? `)`
// CallParams :
//     Expression ( `,` Expression ) * `,` ?
//
// FieldExpression :
//     Expression `.` IDENTIFIER
//
// ReturnExpression :
//     `return` Expression ?
//
// BlockExpression :
//     `{` Statements ? `}`
// Statements :
//     Statement +
//   | Statement + ExpressionWithoutBlock
//   | ExpressionWithoutBlock
//
// LoopExpression :
//     InfiniteLoopExpression
//   | PredicateLoopExpression
//
// InfiniteLoopExpression :
//     `loop` BlockExpression
// PredicateLoopExpression :
//     `while` Expression BlockExpression
//
// IfExpression :
//     `if` Expression BlockExpression
//     ( `else` ( BlockExpression | IfExpression ) ) ?
//
// BorrowExpression :
//     ( `&` | `&&` ) Expression
// DereferenceExpression :
//     `*` Expression
// NegationExpression :
//     `-` Expression
//   | `!` Expression
// ArithmeticOrLogicalExpression :
//     Expression `+` Expression
//   | Expression `-` Expression
//   | Expression `*` Expression
//   | Expression `/` Expression
//   | Expression `%` Expression
//   | Expression `&` Expression
//   | Expression `|` Expression
//   | Expression `^` Expression
//   | Expression `<<` Expression
//   | Expression `>>` Expression
// ComparisonExpression :
//     Expression `==` Expression
//   | Expression `!=` Expression
//   | Expression `>` Expression
//   | Expression `<` Expression
//   | Expression `>=` Expression
//   | Expression `<=` Expression
// LazyBooleanExpression :
//     Expression `||` Expression
//   | Expression `&&` Expression
// TypeCastExpression :
//     Expression `as` TypeNoBounds
// AssignmentExpression :
//     Expression `=` Expression
// CompoundAssignmentExpression :
//     Expression `+=` Expression
//   | Expression `-=` Expression
//   | Expression `*=` Expression
//   | Expression `/=` Expression
//   | Expression `%=` Expression
//   | Expression `&=` Expression
//   | Expression `|=` Expression
//   | Expression `^=` Expression
//   | Expression `<<=` Expression
//   | Expression `>>=` Expression
//
// Type :
//     TypeNoBounds
// TypeNoBounds :
//     TYPES
//   | ReferenceType
// ReferenceType :
//     `&` TypeNoBounds
//
//
//
// LINE_COMMENT :
//     `//` ( ~[\ !] | `//` ) ~ `\n` *
//   | `//`
// BLOCK_COMMENT :
//     `/*` ( ~[* !] | ** ) * `*/`
//   | `/**/`
//   | `/***/`
//
// TYPES :
//     PrimitiveTypes :
//         Boolean :
//             `true` | `false`
//         Numeric :
//             `i32`
//         Textual :
//             `char` | `String`
//     UserDefinedTypes :
//         Struct
//
// CHAR_LITERAL :
//     `'` ~[' \ \n \r \t] `'`
//
// STRING_LITERAL :
//     `"` ~["] * `"`
//
// INTEGER_LITERAL :
//     DEC_LITERAL
// DEC_LITERAL :
//     DEC_DIGIT DEC_DIGIT *
// DEC_DIGIT :
//     [0-9]
//
// BOOLEAN_LITERAL :
//     `true`
//   | `false`
//
// IDENTIFIER_OR_KEYWORD :
//     XID_start XID_continue *
//   | `_` XID_continue +
// IDENTIFIER :
//     NON_KEYWORD_IDENTIFIER
// KEYWORD :
//     BUILTIN
//   | KW_AS: `as`
//   | KW_BREAK: `break`
//   | KW_ELSE: `else`
//   | KW_FALSE: `false`
//   | KW_FN: `fn`
//   | KW_IF: `if`
//   | KW_IMPL: `impl`
//   | KW_LET: `let`
//   | KW_LOOP: `loop`
//   | KW_RETURN: `return`
//   | KW_STRUCT: `struct`
//   | KW_TRUE: `true`
//   | KW_WHILE: `while`
// BUILTIN :
//     `print`
//   | `println`
//
//
//
// assign           = logical_or ( ( '=' | binary_assign_op ) Expression ) ?
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
// unary = ( '-' | '!' | '&' | '*' ) ? prefix
//
// prefix = primary ( '.' ident ) *
//
// primary = num
//         | char
//         | String
//         | bool
//         | builtin
//         | ident ( '(' ( expr ( ',' expr ) * ) ? ')' ) ?
//         | ident '{' ( expr ( ',' expr ) * ) ? ',' ? '}'
//         | '(' expr ')'
//

#[derive(Debug)]
struct Parser<'a> {
    g_symbol_table: &'a mut SymbolTable,  // global symbol table
    current_function: Option<Function>,
    tokens: &'a [Token],
    idx: usize,
    except_struct_expression: bool,
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

pub fn gen_ast<'a>(tokens: &'a [Token], g_symbol_table: &'a mut SymbolTable) -> Program {
    let mut parser = Parser {
        g_symbol_table,
        current_function: None,
        tokens,
        idx: 0,
        except_struct_expression: false,
    };

    program(&mut parser)
}

fn program(mut p: &mut Parser) -> Program {
    let mut program = Program::new();
    while !p.is_eof() {
        if let TokenKind::Keyword(Keyword::Struct) = &p.tokens[p.idx].kind {
            program.structs.push(struct_define(&mut p));
        } else if p.consume(TokenKind::Keyword(Keyword::Impl)) {
            let name = if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
                p.idx += 1;
                name
            } else {
                panic!("expected identifier");
            };
            if program.structs.find_struct(name).is_none() {
                let mut st = Struct::new();
                st.name = name.to_string();
                program.structs.push(st);
            }
            let mut functions = vec![];
            while let TokenKind::Keyword(Keyword::Fn) = &p.tokens[p.idx].kind {
                functions.push(function(&mut p));
            }
            program.structs.find_struct_mut(name).unwrap().functions.append(&mut functions);

        } else if let TokenKind::Keyword(Keyword::Fn) = &p.tokens[p.idx].kind {
            program.functions.push(function(&mut p));
        } else {
            panic!("invalid token: `{:?}`", p.tokens[p.idx].kind);
        }
    }
    program
}

fn struct_define(mut p: &mut Parser) -> Struct {
    let mut st = Struct::new();
    p.expect(TokenKind::Keyword(Keyword::Struct));
    if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
        p.idx += 1;
        st.name = name.to_string();
    } else {
        panic!("expected identifier");
    }
    p.expect(TokenKind::LBrace);
    while !p.consume(TokenKind::RBrace) && !p.is_eof() {
        if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
            p.idx += 1;
            if p.g_symbol_table.find_name(name).is_some() {
                panic!("the name `{}` is defined multiple times", name);
            } else {
                p.expect(TokenKind::Colon);
                let typekind = type_no_bounds(&mut p);
                let obj = Object::new(name.to_string(), st.field.len(), false, typekind);
                st.field.push(obj);
            }
        } else {
            panic!("expected identifier");
        }
        p.consume(TokenKind::Comma);
    }
    st
}

fn function(mut p: &mut Parser) -> Function {
    p.expect(TokenKind::Keyword(Keyword::Fn));
    if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
        if p.g_symbol_table.find_name(name).is_some() {
            panic!("the name `{}` is defined multiple times", name);
        }
        let obj = Rc::new(Object::new(name.to_string(), p.g_symbol_table.len(), false, Type::Void));
        p.g_symbol_table.push(Rc::clone(&obj));
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
                panic!("expected identifier");
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
        let mut node = expr(&mut p);
        while p.consume(TokenKind::Dot) {
            if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
                p.idx += 1;
                node = new_field_node(&mut p.current_function.as_mut().unwrap(), name, node);
            } else {
                panic!("unexpected token: `{:?}`", p.tokens[p.idx].kind);
            }
        }
        node
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
    } else if let TokenKind::Ident(name) = &p.tokens[p.idx].kind {
        p.idx += 1;
        Type::Struct(name.to_string())
    } else {
        panic!("expected type, but got `{:?}`", p.tokens[p.idx].kind);
    }

}

fn block_expression(mut p: &mut Parser) -> Node {
    let mut stmts = vec![];
    p.current_function.as_mut().unwrap().lvar_symbol_table.enter_scope();
    p.expect(TokenKind::LBrace );
    let except_struct_expression = p.except_struct_expression;
    p.except_struct_expression = false;
    while !p.consume(TokenKind::RBrace) && !p.is_eof() {
        stmts.push(statement(&mut p));
    }
    p.except_struct_expression = except_struct_expression;
    p.current_function.as_mut().unwrap().lvar_symbol_table.leave_scope();
    new_block_node(stmts)
}

fn expr(mut p: &mut Parser) -> Node {
    if p.tokens[p.idx].kind == TokenKind::LBrace {
        block_expression(&mut p)
    } else if p.consume(TokenKind::Keyword(Keyword::If)) {
        p.except_struct_expression = true;
        let cond = expr(&mut p);
        p.except_struct_expression = false;
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
        let except_struct_expression = p.except_struct_expression;
        p.except_struct_expression = false;
        let node = expr(&mut p);
        p.expect(TokenKind::RParen);
        p.except_struct_expression = except_struct_expression;
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
            } else if !p.except_struct_expression && p.consume(TokenKind::LBrace) {
                // struct
                let mut field = vec![];
                while !p.consume(TokenKind::RBrace) {
                    field.push(expr(&mut p));
                    p.consume(TokenKind::Comma);
                }
                new_struct_expr_node(&mut p.current_function.as_mut().unwrap().lvar_symbol_table, name, field)
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
