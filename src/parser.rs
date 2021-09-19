use std::rc::Rc;
use super::ast::*;
use super::token::*;
use super::keyword::*;
use super::object::*;
use super::function::*;
use super::program::*;
use super::class::*;
use super::error::*;

// Grammar
//
// Program = Item *
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
// BlockExpression :
//     `{` Statements ? `}`
// Statements :
//     Statement +
//   | Statement + ExpressionWithoutBlock
//   | ExpressionWithoutBlock
//
// Statement :
//     `;`
//   | // Item
//   | LetStatement
//   | ExpressionStatement
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
// EBNF
//
// wip
//
// parse_assign           = parse_logical_or ( ( '=' | binary_assign_op ) Expression ) ?
// binary_assign_op = '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '^=' | '|=' | '<<=' | '>>='
//
// parse_logical_or  = parse_logical_and ( '||' parse_logical_and ) *
// parse_logical_and = parse_equality ( '&&' parse_equality ) *
//
// parse_equality   = parse_relational ( '==' parse_relational | '!=' parse_relational ) *
// parse_relational = parse_bitor ( '<' parse_bitor | '<=' parse_bitor | '>' parse_bitor | '>=' parse_bitor ) *
//
// parse_bitor  = parse_bitxor ( '|' parse_bitxor ) *
// parse_bitxor = parse_bitand ( '^' parse_bitand ) *
// parse_bitand = parse_shift ( '&' parse_shift ) *
//
// parse_shift = parse_add ( '<<' parse_add | '>>' parse_add ) *
// parse_add   = parse_mul ( '+' parse_mul | '-' parse_mul ) *
// parse_mul   = parse_cast ( '*' parse_cast | '/' parse_cast | '%' parse_cast ) *
// parse_cast  = parse_unary ( 'as' type_no_bounds ) *
// parse_unary = ( '-' | '!' | '&' | '*' ) ? prefix
//
// parse_term = num
//         | char
//         | String
//         | bool
//         | builtin
//         | ident ( '(' ( expr ( ',' expr ) * ) ? ')' ) ?
//         | ident '{' ( expr ( ',' expr ) * ) ? ',' ? '}'
//         | '(' expr ')'
//

pub fn gen_ast<'a>(
    input: &'a str,
    tokens: &'a [Token],
    g_symbol_table: &'a mut SymbolTable
) -> Program {
    let mut parser = Parser {
        lines: input.lines(),
        g_symbol_table,
        current_fn: None,
        current_impl: None,
        tokens,
        idx: 0,
        except_struct_expression: false,
    };

    parser.program()
}

#[derive(Debug)]
pub struct Parser<'a> {
    lines: std::str::Lines<'a>,
    g_symbol_table: &'a mut SymbolTable,  // global symbol table
    current_fn: Option<Function>,
    current_impl: Option<Impl>,
    tokens: &'a [Token],
    idx: usize,
    except_struct_expression: bool,
}

impl<'a> Parser<'a> {
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.tokens[self.idx].kind == kind {
            self.idx += 1;
            true
        } else {
            false
        }
    }

    fn eat_keyword(&mut self, kw: Keyword) -> bool {
        self.eat(TokenKind::Keyword(kw))
    }

    fn eat_ident(&mut self) -> Option<String> {
        if let TokenKind::Ident(ident) = &self.tokens[self.idx].kind {
            self.idx += 1;
            Some(ident.to_string())
        } else {
            None
        }
    }

    fn expect(&mut self, kind: TokenKind) {
        if self.tokens[self.idx].kind == kind {
            self.idx += 1;
        } else {
            e0001(self.lines.clone(), self.token(), kind);
        }
    }

    fn expect_ident(&mut self) -> String {
        if let Some(ident) = self.eat_ident() {
            ident
        } else {
            e0003(self.lines.clone(), self.token());
        }
    }

    fn is_eof(&self) -> bool {
        self.tokens[self.idx].kind == TokenKind::Eof
    }

    fn token(&self) -> &Token {
        &self.tokens[self.idx]
    }

    fn current_fn(&self) -> &Function {
        self.current_fn.as_ref().unwrap()
    }

    fn current_fn_mut(&mut self) -> &mut Function {
        self.current_fn.as_mut().unwrap()
    }

    fn type_no_bounds(&mut self) -> Type {
        if self.eat(TokenKind::And) {
            Type::Ptr(Box::new(self.type_no_bounds()))
        } else if self.eat(TokenKind::AndAnd) {
            Type::Ptr(Box::new(Type::Ptr(Box::new(self.type_no_bounds()))))
        } else if let TokenKind::Type(ty) = &self.tokens[self.idx].kind {
            self.idx += 1;
            ty.clone()
        } else if let Some(ident) = self.eat_ident() {
            Type::Struct(ident)
        } else if self.eat_keyword(Keyword::SelfLower) {
            self.current_fn_mut().is_static = false;
            Type::_Self(self.current_impl.as_ref().unwrap().name.to_string())
        } else {
            e0002(self.lines.clone(), self.token());
        }

    }

    fn program(&mut self) -> Program {
        let mut program = Program::new();
        while !self.is_eof() {
            if self.eat_keyword(Keyword::Struct) {
                let st = self.parse_item_struct();
                if program.find_struct(&st.name).is_some() {
                    e0005(self.lines.clone(), self.token(), &st.name);
                }
                program.push_struct(st);
            } else if self.eat_keyword(Keyword::Impl) {
                program.push_or_merge_impl(self.parse_item_impl());
            } else if self.eat_keyword(Keyword::Fn) {
                let f = self.parse_item_fn();
                if program.find_fn(&f.name).is_some() {
                    e0005(self.lines.clone(), self.token(), &f.name);
                }
                program.push_fn(f);
            } else {
                e0004(self.lines.clone(), self.token());
            }
        }
        program
    }

    fn parse_item_struct(&mut self) -> Struct {
        let mut st = Struct::new();
        st.name = self.expect_ident();
        self.expect(TokenKind::LBrace);
        while !self.eat(TokenKind::RBrace) {
            let ident = self.expect_ident();
            if st.field.iter().find(|o|o.name==ident).is_some() {
                e0005(self.lines.clone(), self.token(), &ident);
            } else {
                self.expect(TokenKind::Colon);
                let ty = self.type_no_bounds();
                let obj = Object::new(ident, st.field.len(), false, ty);
                st.field.push(obj);
            }
            self.eat(TokenKind::Comma);
        }
        st
    }

    fn parse_item_impl(&mut self) -> Impl {
        let ident = self.expect_ident();
        self.current_impl = Some(Impl::new(ident));
        self.expect(TokenKind::LBrace);
        while self.eat_keyword(Keyword::Fn) {
            let func = self.parse_item_fn();
            self.current_impl.as_mut().unwrap()
                .functions
                .push(func);
        }
        self.expect(TokenKind::RBrace);
        self.current_impl.take().unwrap()
    }

    fn parse_item_fn(&mut self) -> Function {
        let ident = self.expect_ident();
        let obj = Rc::new(Object::new(ident.to_string(), self.g_symbol_table.len(), false, Type::Void));
        self.g_symbol_table.push(Rc::clone(&obj));
        self.current_fn = Some(Function::new(&ident));

        self.expect(TokenKind::LParen);
        if !self.eat(TokenKind::RParen) {
            while !self.eat(TokenKind::RParen) {
                self.parse_fn_params();
            }
        }

        if self.eat(TokenKind::RArrow) {
            self.parse_ret_ty();
        }

        self.expect(TokenKind::LBrace);
        self.current_fn_mut().statements = self.parse_block_expr();
        self.current_fn.take().unwrap()
    }

    fn parse_fn_params(&mut self) {
        let ident = self.expect_ident();
        if self.current_fn_mut().param_symbol_table.find(&ident).is_some() {
            e0005(self.lines.clone(), self.token(), &ident);
        } else {
            self.expect(TokenKind::Colon);
            let ty = self.type_no_bounds();
            let obj = Rc::new(Object::new(ident, self.current_fn().param_symbol_table.len(), true, ty));
            self.current_fn_mut().param_symbol_table.push(Rc::clone(&obj));
        }
        self.eat(TokenKind::Comma);
    }

    fn parse_ret_ty(&mut self) {
        if let TokenKind::Type(ty) = &self.tokens[self.idx].kind {
            self.idx += 1;
            self.current_fn_mut().rettype = ty.clone();
        } else {
            e0002(self.lines.clone(), self.token());
        }
    }

    fn parse_block_expr(&mut self) -> Node {
        self.current_fn_mut().lvar_symbol_table.enter_scope();
        let except_struct_expression = self.except_struct_expression;
        self.except_struct_expression = false;
        if self.eat(TokenKind::RBrace) {
            return Node::Empty;
        }
        let mut stmts = vec![];
        while !self.eat(TokenKind::RBrace) {
            stmts.push(self.parse_stmt());
        }
        self.except_struct_expression = except_struct_expression;
        self.current_fn_mut().lvar_symbol_table.leave_scope();
        new_block_node(stmts)
    }

    fn parse_stmt(&mut self) -> Node {
        if self.eat(TokenKind::Semi) {
            new_empty_node()
        } else if self.eat_keyword(Keyword::Let) {
            self.parse_let_stmt()
        } else if self.eat_keyword(Keyword::Return) {
            if self.eat(TokenKind::Semi) {
                new_return_node(None)
            } else {
                new_return_node(Some(self.parse_expr()))
            }
        } else {
            let node = self.parse_expr();
            if self.eat(TokenKind::Semi) {
                new_semi_node(node)
            } else {
                node
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Node {
        if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
            self.idx += 1;
            self.expect(TokenKind::Colon);
            let ty = self.type_no_bounds();
            let node = new_variable_node_with_let(&mut self.current_fn_mut().lvar_symbol_table, name, ty);
            if self.eat(TokenKind::Assign) {
                let node = new_assign_node(node, self.parse_expr());
                self.expect(TokenKind::Semi);
                node
            } else {
                self.expect(TokenKind::Semi);
                new_empty_node()
            }
        } else {
            e0003(self.lines.clone(), self.token());
        }
    }

    fn parse_expr(&mut self) -> Node {
        if self.eat(TokenKind::LBrace) {
            self.parse_block_expr()
        } else if self.eat_keyword(Keyword::If) {
            let cond = self.parse_cond();
            self.expect(TokenKind::LBrace );
            let then = self.parse_block_expr();
            let els = if self.eat_keyword(Keyword::Else) {
                self.expect(TokenKind::LBrace );
                Some(self.parse_block_expr())
            } else {
                None
            };
            new_if_node(cond, then, els)
        } else if self.eat_keyword(Keyword::While) {
            let cond = self.parse_cond();
            self.expect(TokenKind::LBrace );
            let then = self.parse_block_expr();
            new_while_node(cond, then)
        } else if self.eat_keyword(Keyword::Loop) {
            self.expect(TokenKind::LBrace );
            new_loop_node(self.parse_block_expr())
        } else {
            self.parse_assign()
        }
    }

    fn parse_cond(&mut self) -> Node {
        self.except_struct_expression = true;
        let node = self.parse_expr();
        self.except_struct_expression = false;
        node
    }

    fn parse_assign(&mut self) -> Node {
        let node = self.parse_logical_or();

        if self.eat(TokenKind::Assign) {
            new_assign_node(node, self.parse_expr())
        } else if self.eat(TokenKind::PlusEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Add, node, self.parse_expr()))
        } else if self.eat(TokenKind::MinusEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Sub, node, self.parse_expr()))
        } else if self.eat(TokenKind::StarEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Mul, node, self.parse_expr()))
        } else if self.eat(TokenKind::SlashEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Div, node, self.parse_expr()))
        } else if self.eat(TokenKind::PercentEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Rem, node, self.parse_expr()))
        } else if self.eat(TokenKind::AndEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitAnd, node, self.parse_expr()))
        } else if self.eat(TokenKind::CaretEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitXor, node, self.parse_expr()))
        } else if self.eat(TokenKind::OrEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitOr, node, self.parse_expr()))
        } else if self.eat(TokenKind::ShlEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Shl, node, self.parse_expr()))
        } else if self.eat(TokenKind::ShrEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Shr, node, self.parse_expr()))
        } else {
            node
        }
    }

    fn parse_logical_or(&mut self) -> Node {
        let mut node = self.parse_logical_and();

        loop {
            if self.eat(TokenKind::OrOr) {
                node = new_short_circuit_op_node(ShortCircuitOpKind::Or, node, self.parse_logical_and());
            } else {
                return node;
            }
        }
    }

    fn parse_logical_and(&mut self) -> Node {
        let mut node = self.parse_equality();

        loop {
            if self.eat(TokenKind::AndAnd) {
                node = new_short_circuit_op_node(ShortCircuitOpKind::And, node, self.parse_equality());
            } else {
                return node;
            }
        }
    }

    fn parse_equality(&mut self) -> Node {
        let mut node = self.parse_relational();

        loop {
            if self.eat(TokenKind::EqEq) {
                node = new_binary_op_node(BinaryOpKind::Eq, node, self.parse_relational());
            } else if self.eat(TokenKind::Ne) {
                node = new_binary_op_node(BinaryOpKind::Ne, node, self.parse_relational());
            } else {
                return node;
            }
        }
    }

    fn parse_relational(&mut self) -> Node {
        let mut node = self.parse_bitor();

        loop {
            if self.eat(TokenKind::Lt) {
                node = new_binary_op_node(BinaryOpKind::Lt, node, self.parse_bitor());
            } else if self.eat(TokenKind::Le) {
                node = new_binary_op_node(BinaryOpKind::Le, node, self.parse_bitor());
            } else if self.eat(TokenKind::Gt) {
                node = new_binary_op_node(BinaryOpKind::Gt, node, self.parse_bitor());
            } else if self.eat(TokenKind::Ge) {
                node = new_binary_op_node(BinaryOpKind::Ge, node, self.parse_bitor());
            } else {
                return node;
            }
        }
    }

    fn parse_bitor(&mut self) -> Node {
        let mut node = self.parse_bitxor();
        while self.eat(TokenKind::Or) {
            node = new_binary_op_node(BinaryOpKind::BitOr, node, self.parse_bitxor());
        }
        node
    }

    fn parse_bitxor(&mut self) -> Node {
        let mut node = self.parse_bitand();
        while self.eat(TokenKind::Caret) {
            node = new_binary_op_node(BinaryOpKind::BitXor, node, self.parse_bitand());
        }
        node
    }

    fn parse_bitand(&mut self) -> Node {
        let mut node = self.parse_shift();
        while self.eat(TokenKind::And) {
            node = new_binary_op_node(BinaryOpKind::BitAnd, node, self.parse_shift());
        }
        node
    }

    fn parse_shift(&mut self) -> Node {
        let mut node = self.parse_add();

        loop {
            if self.eat(TokenKind::Shl) {
                node = new_binary_op_node(BinaryOpKind::Shl, node, self.parse_add());
            } else if self.eat(TokenKind::Shr) {
                node = new_binary_op_node(BinaryOpKind::Shr, node, self.parse_add());
            } else {
                return node;
            }
        }
    }

    fn parse_add(&mut self) -> Node {
        let mut node = self.parse_mul();

        loop {
            if self.eat(TokenKind::Plus) {
                node = new_binary_op_node(BinaryOpKind::Add, node, self.parse_mul());
            } else if self.eat(TokenKind::Minus) {
                node = new_binary_op_node(BinaryOpKind::Sub, node, self.parse_mul());
            } else {
                return node;
            }
        }
    }

    fn parse_mul(&mut self) -> Node {
        let mut node = self.parse_cast();

        loop {
            if self.eat(TokenKind::Star) {
                node = new_binary_op_node(BinaryOpKind::Mul, node, self.parse_cast());
            } else if self.eat(TokenKind::Slash) {
                node = new_binary_op_node(BinaryOpKind::Div, node, self.parse_cast());
            } else if self.eat(TokenKind::Percent) {
                node = new_binary_op_node(BinaryOpKind::Rem, node, self.parse_cast());
            } else {
                return node;
            }
        }
    }

    fn parse_cast(&mut self) -> Node {
        let mut node = self.parse_unary();

        loop {
            if self.eat_keyword(Keyword::As) {
                let ty = self.type_no_bounds();
                node = new_cast_node(ty, node);
            } else {
                return node;
            }
        }
    }

    fn parse_unary(&mut self) -> Node {
        if self.eat(TokenKind::Minus) {
            new_unary_op_node(UnaryOpKind::Neg, self.parse_unary())
        } else if self.eat(TokenKind::Not) {
            new_unary_op_node(UnaryOpKind::Not, self.parse_unary())
        } else if self.eat(TokenKind::And) {
            new_unary_op_node(UnaryOpKind::Ref, self.parse_unary())
        } else if self.eat(TokenKind::Star) {
            new_unary_op_node(UnaryOpKind::Deref, self.parse_unary())
        } else {
            self.parse_field_or_method_expr()
        }
    }

    fn parse_field_or_method_expr(&mut self) -> Node {
        let mut node = self.parse_term();

        loop {
            if self.eat(TokenKind::Dot) {
                let ident = self.expect_ident();
                if self.eat(TokenKind::LParen) {
                    // method
                    let mut args = vec![];
                    while !self.eat(TokenKind::RParen) {
                        args.push(self.parse_expr());
                        self.eat(TokenKind::Comma);
                    }
                    node = new_method_call_node(node, ident, args);
                } else {
                    // field
                    node = new_field_node(node, ident);
                }
            } else {
                return node;
            }
        }
    }

    fn parse_term(&mut self) -> Node {
        if self.eat(TokenKind::LParen) {
            let except_struct_expression = self.except_struct_expression;
            self.except_struct_expression = false;
            let node = self.parse_expr();
            self.expect(TokenKind::RParen);
            self.except_struct_expression = except_struct_expression;
            return node;
        } else if self.eat(TokenKind::LBrace) {
            let except_struct_expression = self.except_struct_expression;
            self.except_struct_expression = false;
            let node = self.parse_block_expr();
            self.except_struct_expression = except_struct_expression;
            return node;
        }

        match &self.tokens[self.idx].kind {
            TokenKind::Integer(num) => {
                self.idx += 1;
                new_num_node(*num)
            }
            TokenKind::Char(c) => {
                self.idx += 1;
                new_char_node(*c)
            }
            TokenKind::String(s) => {
                self.idx += 1;
                new_string_node(s)
            }
            TokenKind::Ident(name) => {
                self.idx += 1;
                if self.eat(TokenKind::LParen) {
                    // function
                    let mut args = vec![];
                    while !self.eat(TokenKind::RParen) {
                        args.push(self.parse_expr());
                        self.eat(TokenKind::Comma);
                    }
                    new_function_call_node(name, args)
                } else if !self.except_struct_expression && self.eat(TokenKind::LBrace) {
                    // struct
                    let mut field = vec![];
                    while !self.eat(TokenKind::RBrace) {
                        field.push(self.parse_expr());
                        self.eat(TokenKind::Comma);
                    }
                    new_struct_expr_node(&mut self.current_fn_mut().lvar_symbol_table, name, field)
                } else {
                    // local variable or parameter
                    if let Some(obj) = self.current_fn().lvar_symbol_table.find(name) {
                        new_variable_node(obj)
                    } else if let Some(obj) = self.current_fn().param_symbol_table.find(name) {
                        new_variable_node(obj)
                    } else {
                        e0007(self.lines.clone(), self.token(), name);
                    }
                }
            }
            TokenKind::Keyword(b) if matches!(b, Keyword::True|Keyword::False) => {
                self.idx += 1;
                new_bool_node(*b)
            }
            TokenKind::Builtin(kind) => {
                self.idx += 1;
                self.expect(TokenKind::LParen);
                let mut args = vec![];
                while !self.eat(TokenKind::RParen) {
                    args.push(self.parse_expr());
                    self.eat(TokenKind::Comma);
                }
                new_builtin_call_node(*kind, args)
            }
            _ => {
                e0006(self.lines.clone(), self.token());
            }
        }
    }
}
