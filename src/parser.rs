use crate::ast::*;
use crate::builtin::*;
use crate::class::{Struct, Impl};
use crate::error::*;
use crate::function::Function;
use crate::keyword::{Type, Keyword};
use crate::object::{Object, FindSymbol, SymbolTable};
use crate::program::Program;
use crate::token::{Token, TokenKind, LiteralKind};
use std::cell::RefCell;
use std::rc::Rc;

// Grammar
//
// Program = Item *
//
// Item :
//     Function
//   | Mod
//   | Struct
//   | Implementation
//
// Function :
//     `fn` IDENTIFIER `(` FunctionParameters ? `)` FunctionReturnType ? BlockExpression
// FunctionParameters :
//     SelfParam ? FunctionParam ( `,` FunctionParam ) * `,` ?
// SelfParam:
//     `&self`
// FunctionParam :
//     IDENTIFIER `:` Type
// FunctionReturnType :
//     `->` Type
//
// Mod :
//     `mod` IDENTIFIER ( `{` Item * `}` | `;` )
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

#[derive(Debug)]
pub struct Parser<'a> {
    path: &'a str,
    input: &'a str,
    lines: Vec<&'a str>,
    g_symbol_table: &'a mut SymbolTable,  // global symbol table
    current_fn: Option<Function<'a>>,
    current_impl: Option<Impl<'a>>,
    tokens: &'a [Token],
    idx: usize,
    except_struct_expression: bool,
    brk_label_seq: usize,
    loop_count: usize,
    current_mod: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(
        path: &'a str,
        input: &'a str,
        tokens: &'a [Token],
        g_symbol_table: &'a mut SymbolTable,
    ) -> Self {
        Parser {
            path,
            input,
            lines: input.lines().collect(),
            g_symbol_table,
            current_fn: None,
            current_impl: None,
            tokens,
            idx: 0,
            except_struct_expression: false,
            brk_label_seq: 0,
            loop_count: 0,
            current_mod: vec![],
        }
    }

    pub fn gen_ast(&mut self) -> Program<'a> {
        self.program()
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.tokens[self.idx].kind == kind
    }

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
        if let TokenKind::Identifier(ident) = &self.tokens[self.idx].kind {
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
            e0001(self.errorset(), kind);
        }
    }

    fn expect_ident(&mut self) -> String {
        if let Some(ident) = self.eat_ident() {
            ident
        } else {
            e0003(self.errorset());
        }
    }

    fn is_eof(&self) -> bool {
        self.tokens[self.idx].kind == TokenKind::Eof
    }

    fn current_token(&self) -> &[Token] {
        &self.tokens[self.idx..=self.idx]
    }

    fn current_fn(&self) -> &Function {
        self.current_fn.as_ref().unwrap()
    }

    fn current_fn_mut(&mut self) -> &mut Function<'a> {
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
            // WIP
            if self.check(TokenKind::PathSep) {
                let mut path = vec![ident];
                while self.eat(TokenKind::PathSep) {
                    path.push(self.expect_ident());
                }
                let ident = path.pop().unwrap();
                Type::Struct(path, ident, false)
            } else {
                Type::Struct(self.current_mod.to_vec(), ident, false)
            }
        } else if self.eat_keyword(Keyword::SelfUpper) {
            self.current_fn_mut().is_static = false;
            Type::_Self(self.current_mod.to_vec(), self.current_impl.as_ref().unwrap().name.to_string(), false)
        } else {
            e0002(self.errorset());
        }

    }

    fn inside_of_a_loop(&self) -> bool {
        self.loop_count != 0
    }

    fn seq(&self) -> usize {
        unsafe {
            static mut ID: usize = 0;
            ID += 1;
            ID
        }
    }

    fn errorset(&self) -> (&str, &[&'a str], &[Token]) {
        (self.path, &self.lines, self.current_token())
    }

    fn program(&mut self) -> Program<'a> {
        let mut program = Program::new(self.path, self.input);
        while let Some(item) = self.parse_item() {
            self.parse_program(&mut program, item);
        }
        program
    }

    fn parse_program(&mut self, program: &mut Program<'a>, item: ItemKind<'a>) {
        match item {
            ItemKind::Struct(mut st) => {
                if program.current_namespace.borrow().find_struct(&st.name).is_some() {
                    e0005(self.errorset(), &st.name);
                }
                st.path = program.current_namespace.borrow().full_path();
                program.current_namespace.borrow_mut().push_struct(st);
            }
            ItemKind::Impl(impl_item) => {
                program.current_namespace.borrow_mut().push_impl(impl_item);
            }
            ItemKind::Mod(mod_item) => {
                program.enter_namespace(&mod_item.0);
                for item in mod_item.1 {
                    self.parse_program(program, item);
                }
                program.leave_namespace();
            }
            ItemKind::Fn(f) => {
                if program.current_namespace.borrow().find_fn(&f.name).is_some() {
                    e0005(self.errorset(), &f.name);
                }
                program.current_namespace.borrow_mut().push_fn(f);
            }
        }
    }

    fn parse_item(&mut self) -> Option<ItemKind<'a>> {
        if self.eat_keyword(Keyword::Struct) {
            let st = self.parse_item_struct();
            Some(ItemKind::Struct(st))
        } else if self.eat_keyword(Keyword::Impl) {
            let impl_item = self.parse_item_impl();
            Some(ItemKind::Impl(impl_item))
        } else if self.eat_keyword(Keyword::Mod) {
            let mod_item = self.parse_item_mod();
            Some(ItemKind::Mod(mod_item))
        } else if self.eat_keyword(Keyword::Fn) {
            let f = self.parse_item_fn();
            Some(ItemKind::Fn(f))
        } else if self.is_eof() || self.check(TokenKind::RBrace) {
            None
        } else {
            e0004(self.errorset());
        }
    }

    fn parse_item_mod(&mut self) -> (String, Vec<ItemKind<'a>>) {
        let id = self.expect_ident();
        self.current_mod.push(id.to_string());
        let mod_kind = if self.eat(TokenKind::Semi) {
            // TODO
            todo!("ModKind::Unloaded");
        } else {
            self.expect(TokenKind::LBrace);
            self.parse_mod()
        };
        self.current_mod.pop();
        (id, mod_kind)
    }

    fn parse_mod(&mut self) -> Vec<ItemKind<'a>> {
        let mut items = vec![];
        while let Some(item) = self.parse_item() {
            items.push(item);
        }
        self.expect(TokenKind::RBrace);
        items
    }

    fn parse_item_struct(&mut self) -> Struct<'a> {
        let mut st = Struct::new();
        st.name = self.expect_ident();
        self.expect(TokenKind::LBrace);
        while !self.eat(TokenKind::RBrace) {
            let ident = self.expect_ident();
            if st.field.iter().any(|o|o.name==ident) {
                e0005(self.errorset(), &ident);
            } else {
                self.expect(TokenKind::Colon);
                let ty = self.type_no_bounds();
                let obj = Object::new(ident, st.field.len(), false, ty, false);
                st.field.push(obj);
            }
            if !self.eat(TokenKind::Comma) && !self.check(TokenKind::RBrace) {
                e0008(self.errorset());
            }
        }
        st
    }

    fn parse_item_impl(&mut self) -> Impl<'a> {
        let ident = self.expect_ident();
        self.current_impl = Some(Impl::new(ident));
        self.expect(TokenKind::LBrace);
        while self.eat_keyword(Keyword::Fn) {
            let func = self.parse_item_fn();
            self.current_impl.as_mut().unwrap()
                .functions
                .push(Rc::new(func));
        }
        self.expect(TokenKind::RBrace);
        self.current_impl.take().unwrap()
    }

    fn parse_item_fn(&mut self) -> Function<'a> {
        let ident = self.expect_ident();
        let obj = Rc::new(RefCell::new(Object::new(ident.to_string(), self.g_symbol_table.len(), false, Type::Void, false)));
        self.g_symbol_table.push(Rc::clone(&obj));
        self.current_fn = Some(Function::new(&ident));

        self.expect(TokenKind::LParen);
        if !self.eat(TokenKind::RParen) {
            // TODO: 所有権の実装後に`&`なしの`self`に対応
            if self.eat(TokenKind::And) {
                let is_mutable = self.eat_keyword(Keyword::Mut);
                if self.eat_keyword(Keyword::SelfLower) {
                    // (&self) -> (self: &Self)
                    self.current_fn_mut().is_static = false;
                    let ident = "self".to_string();
                    let ty = Type::_Self(self.current_mod.to_vec(), self.current_impl.as_ref().unwrap().name.to_string(), false);
                    let obj = Rc::new(RefCell::new(Object::new(ident, self.current_fn().param_symbol_table.len(), true, ty, is_mutable)));
                    obj.borrow_mut().assigned = true;
                    self.current_fn_mut().param_symbol_table.push(Rc::clone(&obj));
                    if !self.eat(TokenKind::Comma) && !self.check(TokenKind::RParen) {
                        e0009(self.errorset());
                    }
                } else {
                    e0000((self.path, &self.lines, &self.tokens[self.idx-1..=self.idx]),
                        "variable declaration cannot be a reference");
                }
            }
            while !self.eat(TokenKind::RParen) {
                self.parse_fn_params();
                if !self.eat(TokenKind::Comma) && !self.check(TokenKind::RParen) {
                    e0009(self.errorset());
                }
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
        if self.eat(TokenKind::And) {
            e0000((self.path, &self.lines, &self.tokens[self.idx-1..=self.idx]),
                "variable declaration cannot be a reference");
        }
        let is_mutable = self.eat_keyword(Keyword::Mut);
        let ident = self.expect_ident();
        if self.current_fn_mut().param_symbol_table.find(&ident).is_some() {
            e0005(self.errorset(), &ident);
        } else {
            self.expect(TokenKind::Colon);
            let ty = self.type_no_bounds();
            let obj = Rc::new(RefCell::new(Object::new(ident, self.current_fn().param_symbol_table.len(), true, ty, is_mutable)));
            obj.borrow_mut().assigned = true;
            self.current_fn_mut().param_symbol_table.push(Rc::clone(&obj));
        }
    }

    fn parse_ret_ty(&mut self) {
        self.current_fn_mut().rettype = self.type_no_bounds();
    }

    fn parse_block_expr(&mut self) -> Node<'a> {
        self.current_fn_mut().lvar_symbol_table.enter_scope();
        let except_struct_expression = self.except_struct_expression;
        self.except_struct_expression = false;
        let begin = self.idx;
        if self.eat(TokenKind::RBrace) {
            return new_empty_node()
        }
        let mut stmts = vec![];
        while !self.eat(TokenKind::RBrace) {
            stmts.push(self.parse_stmt());
        }
        self.except_struct_expression = except_struct_expression;
        self.current_fn_mut().lvar_symbol_table.leave_scope();
        new_block_node(stmts, &self.tokens[begin..self.idx])
    }

    fn parse_stmt(&mut self) -> Node<'a> {
        if self.eat(TokenKind::Semi) {
            new_empty_node()
        } else if self.eat_keyword(Keyword::Let) {
            self.parse_let_stmt()
        } else if self.eat_keyword(Keyword::Return) {
            self.parse_return_expr()
        } else if self.eat_keyword(Keyword::Break) {
            self.parse_break_expr()
        } else {
            let begin = self.idx;
            let node = self.parse_expr();
            if self.eat(TokenKind::Semi) {
                new_semi_node(node, &self.tokens[begin..self.idx])
            } else {
                node
            }
        }
    }

    fn parse_return_expr(&mut self) -> Node<'a> {
        let begin = self.idx;
        if self.eat(TokenKind::Semi) {
            new_return_node(None, &self.tokens[begin..self.idx])
        } else {
            let node = new_return_node(
                Some(self.parse_expr()),
                &self.tokens[begin..self.idx],
            );
            self.eat(TokenKind::Semi);
            new_return_node(Some(node), &self.tokens[begin..self.idx])
        }
    }

    fn parse_break_expr(&mut self) -> Node<'a> {
        if !self.inside_of_a_loop() {
            e0000((self.path, &self.lines, &self.tokens[self.idx-1..self.idx]),
                "cannot `break` outside of a loop");
        }
        let begin = self.idx;
        if self.eat(TokenKind::Semi) {
            new_break_node(self.brk_label_seq, &self.tokens[begin..self.idx])
        } else {
            unimplemented!();
        }
    }

    fn parse_let_stmt(&mut self) -> Node<'a> {
        let begin = self.idx;
        let is_mutable = self.eat_keyword(Keyword::Mut);
        let ident = self.expect_ident();
        self.expect(TokenKind::Colon);
        let ty = self.type_no_bounds();
        let token = &self.tokens[begin..self.idx];
        let node = new_variable_node_with_let(
            &mut self.current_fn_mut().lvar_symbol_table,
            ident,
            ty,
            token,
            is_mutable,
        );
        if self.eat(TokenKind::Assign) {
            let node = new_assign_node(
                node,
                self.parse_expr(),
                &self.tokens[begin..self.idx],
            );
            self.expect(TokenKind::Semi);
            node
        } else {
            self.expect(TokenKind::Semi);
            new_empty_node()
        }
    }

    fn parse_expr(&mut self) -> Node<'a> {
        if self.eat(TokenKind::LBrace) {
            self.parse_block_expr()
        } else if self.eat_keyword(Keyword::If) {
            self.parse_if_expr()
        } else if self.eat_keyword(Keyword::Loop) {
            self.parse_infinite_loop_expr()
        } else if self.eat_keyword(Keyword::While) {
            self.parse_predicate_loop_expr()
        } else {
            self.parse_assign()
        }
    }

    fn parse_if_expr(&mut self) -> Node<'a> {
        let begin = self.idx - 1;
        let cond = self.parse_cond();
        self.expect(TokenKind::LBrace);
        let then = self.parse_block_expr();
        let els = if self.eat_keyword(Keyword::Else) {
            self.expect(TokenKind::LBrace);
            Some(self.parse_block_expr())
        } else {
            None
        };
        new_if_node(cond, then, els, &self.tokens[begin..self.idx])
    }

    fn parse_infinite_loop_expr(&mut self) -> Node<'a> {
        let begin = self.idx;
        let tmp = self.brk_label_seq;
        self.brk_label_seq = self.seq();
        let brk_label_seq = self.brk_label_seq;
        self.loop_count += 1;
        self.expect(TokenKind::LBrace);
        let then = self.parse_block_expr();
        self.loop_count -= 1;
        self.brk_label_seq = tmp;
        new_loop_node(then, brk_label_seq, &self.tokens[begin..self.idx])
    }

    fn parse_predicate_loop_expr(&mut self) -> Node<'a> {
        let begin = self.idx;
        let tmp = self.brk_label_seq;
        self.brk_label_seq = self.seq();
        let brk_label_seq = self.brk_label_seq;
        let cond = self.parse_cond();
        self.loop_count += 1;
        self.expect(TokenKind::LBrace);
        let then = self.parse_block_expr();
        self.loop_count -= 1;
        self.brk_label_seq = tmp;
        new_while_node(cond, then, brk_label_seq, &self.tokens[begin..self.idx])
    }

    fn parse_cond(&mut self) -> Node<'a> {
        self.except_struct_expression = true;
        let node = self.parse_expr();
        self.except_struct_expression = false;
        node
    }

    fn parse_assign(&mut self) -> Node<'a> {
        let begin = self.idx;
        let node = self.parse_logical_or();

        if self.eat(TokenKind::Assign) {
            new_assign_node(
                node,
                self.parse_expr(),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::PlusEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::Add,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::MinusEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::Sub,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::StarEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::Mul,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::SlashEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::Div,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::PercentEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::Rem,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::AndEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::BitAnd,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::CaretEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::BitXor,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::OrEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::BitOr,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::ShlEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::Shl,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::ShrEq) {
            let begin2 = self.idx;
            let lhs = node.clone();
            new_assign_node(
                lhs,
                new_binary_op_node(
                    BinaryOpKind::Shr,
                    node,
                    self.parse_expr(),
                    &self.tokens[begin2..self.idx],
                ),
                &self.tokens[begin..self.idx],
            )
        } else {
            node
        }
    }

    fn parse_logical_or(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_logical_and();

        loop {
            if self.eat(TokenKind::OrOr) {
                node = new_short_circuit_op_node(
                    ShortCircuitOpKind::Or,
                    node,
                    self.parse_logical_and(),
                    &self.tokens[begin..self.idx],
                );
            } else {
                return node;
            }
        }
    }

    fn parse_logical_and(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_equality();

        loop {
            if self.eat(TokenKind::AndAnd) {
                node = new_short_circuit_op_node(
                    ShortCircuitOpKind::And,
                    node,
                    self.parse_logical_and(),
                    &self.tokens[begin..self.idx],
                );
            } else {
                return node;
            }
        }
    }

    fn parse_equality(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_relational();

        loop {
            if self.eat(TokenKind::EqEq) {
                node = new_binary_op_node(
                    BinaryOpKind::Eq,
                    node,
                    self.parse_relational(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Ne) {
                node = new_binary_op_node(
                    BinaryOpKind::Ne,
                    node,
                    self.parse_relational(),
                    &self.tokens[begin..self.idx],
                );
            } else {
                return node;
            }
        }
    }

    fn parse_relational(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_bitor();

        loop {
            if self.eat(TokenKind::Lt) {
                node = new_binary_op_node(
                    BinaryOpKind::Lt,
                    node,
                    self.parse_bitor(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Le) {
                node = new_binary_op_node(
                    BinaryOpKind::Le,
                    node,
                    self.parse_bitor(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Gt) {
                node = new_binary_op_node(
                    BinaryOpKind::Gt,
                    node,
                    self.parse_bitor(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Ge) {
                node = new_binary_op_node(
                    BinaryOpKind::Ge,
                    node,
                    self.parse_bitor(),
                    &self.tokens[begin..self.idx],
                );
            } else {
                return node;
            }
        }
    }

    fn parse_bitor(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_bitxor();
        while self.eat(TokenKind::Or) {
            node = new_binary_op_node(
                BinaryOpKind::BitOr,
                node,
                self.parse_bitxor(),
                &self.tokens[begin..self.idx],
            );
        }
        node
    }

    fn parse_bitxor(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_bitand();
        while self.eat(TokenKind::Caret) {
            node = new_binary_op_node(
                BinaryOpKind::BitXor,
                node,
                self.parse_bitand(),
                &self.tokens[begin..self.idx],
            );
        }
        node
    }

    fn parse_bitand(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_shift();
        while self.eat(TokenKind::And) {
            node = new_binary_op_node(
                BinaryOpKind::BitAnd,
                node,
                self.parse_shift(),
                &self.tokens[begin..self.idx],
            );
        }
        node
    }

    fn parse_shift(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_add();

        loop {
            if self.eat(TokenKind::Shl) {
                node = new_binary_op_node(
                    BinaryOpKind::Shl,
                    node,
                    self.parse_add(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Shr) {
                node = new_binary_op_node(
                    BinaryOpKind::Shr,
                    node,
                    self.parse_add(),
                    &self.tokens[begin..self.idx],
                );
            } else {
                return node;
            }
        }
    }

    fn parse_add(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_mul();

        loop {
            if self.eat(TokenKind::Plus) {
                node = new_binary_op_node(
                    BinaryOpKind::Add,
                    node,
                    self.parse_mul(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Minus) {
                node = new_binary_op_node(
                    BinaryOpKind::Sub,
                    node,
                    self.parse_mul(),
                    &self.tokens[begin..self.idx],
                );
            } else {
                return node;
            }
        }
    }

    fn parse_mul(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_cast();

        loop {
            if self.eat(TokenKind::Star) {
                node = new_binary_op_node(
                    BinaryOpKind::Mul,
                    node,
                    self.parse_cast(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Slash) {
                node = new_binary_op_node(
                    BinaryOpKind::Div,
                    node,
                    self.parse_cast(),
                    &self.tokens[begin..self.idx],
                );
            } else if self.eat(TokenKind::Percent) {
                node = new_binary_op_node(
                    BinaryOpKind::Rem,
                    node,
                    self.parse_cast(),
                    &self.tokens[begin..self.idx],
                );
            } else {
                return node;
            }
        }
    }

    fn parse_cast(&mut self) -> Node<'a> {
        let begin = self.idx;
        let mut node = self.parse_unary();

        loop {
            if self.eat_keyword(Keyword::As) {
                let ty = self.type_no_bounds();
                node = new_cast_node(ty, node, &self.tokens[begin..self.idx]);
            } else {
                return node;
            }
        }
    }

    fn parse_unary(&mut self) -> Node<'a> {
        let begin = self.idx;
        if self.eat(TokenKind::Minus) {
            new_unary_op_node(
                UnaryOpKind::Neg,
                self.parse_unary(),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::Not) {
            new_unary_op_node(
                UnaryOpKind::Not,
                self.parse_unary(),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::And) {
            new_unary_op_node(
                UnaryOpKind::Ref,
                self.parse_unary(),
                &self.tokens[begin..self.idx],
            )
        } else if self.eat(TokenKind::Star) {
            new_unary_op_node(
                UnaryOpKind::Deref,
                self.parse_unary(),
                &self.tokens[begin..self.idx],
            )
        } else {
            self.parse_field_or_method_expr()
        }
    }

    fn parse_field_or_method_expr(&mut self) -> Node<'a> {
        let mut node = self.parse_term();

        loop {
            if self.eat(TokenKind::Dot) {
                let begin = self.idx;
                let ident = self.expect_ident();
                if self.eat(TokenKind::LParen) {
                    // method
                    let mut args = vec![];
                    while !self.eat(TokenKind::RParen) {
                        args.push(self.parse_expr());
                        if !self.eat(TokenKind::Comma) && !self.check(TokenKind::RParen) {
                            e0010(self.errorset());
                        }
                    }
                    node = new_method_call_node(
                        node,
                        ident,
                        args,
                        &self.tokens[begin..self.idx]
                    );
                } else {
                    // field
                    node = new_field_node(node, ident, &self.tokens[begin..self.idx]);
                }
            } else {
                return node;
            }
        }
    }

    fn parse_term(&mut self) -> Node<'a> {
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
            TokenKind::Literal(lit) => {
                self.idx += 1;
                self.parse_lit(lit)
            }
            TokenKind::Identifier(name) => {
                self.idx += 1;
                self.parse_ident(name)
            }
            TokenKind::Keyword(b) if matches!(b, Keyword::True|Keyword::False) => {
                self.idx += 1;
                new_bool_node(*b, &self.tokens[self.idx-1..self.idx])
            }
            TokenKind::Keyword(Keyword::SelfLower) => {
                self.idx += 1;
                self.parse_ident("self")
            }
            TokenKind::Builtin(kind) => {
                self.idx += 1;
                self.parse_builtin(kind)
            }
            _ => {
                e0006(self.errorset());
            }
        }
    }

    fn parse_lit(&mut self, kind: &LiteralKind) -> Node<'a> {
        match kind {
            LiteralKind::Char(c) => {
                new_char_node(*c, &self.tokens[self.idx-1..=self.idx-1])
            }
            LiteralKind::String(s) => {
                new_string_node(s, &self.tokens[self.idx-1..=self.idx-1])
            }
            LiteralKind::Integer(i) => {
                new_num_node(*i, &self.tokens[self.idx-1..=self.idx-1])
            }
        }
    }

    fn parse_ident(&mut self, name: &str) -> Node<'a> {
        if self.eat(TokenKind::PathSep) {
            self.parse_simple_path(name)
        } else if self.eat(TokenKind::LParen) {
            // function
            let begin = self.idx-2;
            let mut args = vec![];
            while !self.eat(TokenKind::RParen) {
                args.push(self.parse_expr());
                if !self.eat(TokenKind::Comma) && !self.check(TokenKind::RParen) {
                    e0010(self.errorset());
                }
            }
            new_function_call_node(name, args, &self.tokens[begin..self.idx])
        } else if !self.except_struct_expression && self.eat(TokenKind::LBrace) {
            // struct
            let begin = self.idx-2;
            let mut field = vec![];
            while !self.eat(TokenKind::RBrace) {
                field.push(self.parse_expr());
                if !self.eat(TokenKind::Comma) && !self.check(TokenKind::RBrace) {
                    e0009(self.errorset());
                }
            }
            let tokens = &self.tokens[begin..self.idx];
            let current_mod = self.current_mod.to_vec();
            new_struct_expr_node(
                &mut self.current_fn_mut().lvar_symbol_table,
                name,
                field,
                tokens,
                current_mod,
            )
        } else {
            // local variable or parameter
            if let Some(obj) = self.current_fn().lvar_symbol_table.find(name) {
                new_variable_node(obj, &self.tokens[self.idx-1..self.idx])
            } else if let Some(obj) = self.current_fn().param_symbol_table.find(name) {
                new_variable_node(obj, &self.tokens[self.idx-1..self.idx])
            } else {
                e0007(self.errorset(), name);
            }
        }
    }

    fn parse_builtin(&mut self, kind: &Builtin) -> Node<'a> {
        let begin = self.idx-1;
        self.expect(TokenKind::Not);
        self.expect(TokenKind::LParen);
        let mut args = vec![];
        while !self.eat(TokenKind::RParen) {
            args.push(self.parse_expr());
            if !self.eat(TokenKind::Comma) && !self.check(TokenKind::RParen) {
                e0010(self.errorset());
            }
        }
        new_builtin_call_node(*kind, args, &self.tokens[begin..self.idx])
    }

    fn parse_simple_path(&mut self, segment: &str) -> Node<'a> {
        let begin = self.idx;
        let ident = self.expect_ident();
        if self.eat(TokenKind::PathSep) {
            new_path_node(
                segment,
                self.parse_simple_path(&ident),
                &self.tokens[begin..self.idx],
            )
        } else {
            new_path_node(
                segment,
                self.parse_ident(&ident),
                &self.tokens[begin..self.idx],
            )
        }
    }
}
