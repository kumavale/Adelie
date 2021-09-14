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
// Statement :
//     `;`
//   | Item
//   | LetStatement
//   | ExpressionStatement
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

pub fn gen_ast<'a>(tokens: &'a [Token], g_symbol_table: &'a mut SymbolTable) -> Program {
    let mut parser = Parser {
        g_symbol_table,
        current_function: None,
        tokens,
        idx: 0,
        except_struct_expression: false,
    };

    parser.program()
}

#[derive(Debug)]
struct Parser<'a> {
    g_symbol_table: &'a mut SymbolTable,  // global symbol table
    current_function: Option<Function>,
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

    fn program(&mut self) -> Program {
        let mut program = Program::new();
        while !self.is_eof() {
            if self.eat_keyword(Keyword::Struct) {
                program.structs.push(self.struct_define());
            } else if self.eat_keyword(Keyword::Impl) {
                let name = if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
                    self.idx += 1;
                    name
                } else {
                    panic!("expected identifier");
                };
                if program.structs.find(name).is_none() {
                    let mut st = Struct::new();
                    st.name = name.to_string();
                    program.structs.push(st);
                }
                let mut functions = vec![];
                while self.eat_keyword(Keyword::Fn) {
                    functions.push(self.function());
                }
                program.structs.find_mut(name).unwrap().functions.append(&mut functions);

            } else if self.eat_keyword(Keyword::Fn) {
                program.functions.push(self.function());
            } else {
                panic!("invalid token: `{:?}`", self.tokens[self.idx].kind);
            }
        }
        program
    }

    fn struct_define(&mut self) -> Struct {
        let mut st = Struct::new();
        if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
            self.idx += 1;
            st.name = name.to_string();
        } else {
            panic!("expected identifier");
        }
        self.expect(TokenKind::LBrace);
        while !self.eat(TokenKind::RBrace) && !self.is_eof() {
            if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
                self.idx += 1;
                if self.g_symbol_table.find(name).is_some() {
                    panic!("the name `{}` is defined multiple times", name);
                } else {
                    self.expect(TokenKind::Colon);
                    let typekind = self.type_no_bounds();
                    let obj = Object::new(name.to_string(), st.field.len(), false, typekind);
                    st.field.push(obj);
                }
            } else {
                panic!("expected identifier");
            }
            self.eat(TokenKind::Comma);
        }
        st
    }

    fn function(&mut self) -> Function {
        if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
            if self.g_symbol_table.find(name).is_some() {
                panic!("the name `{}` is defined multiple times", name);
            }
            let obj = Rc::new(Object::new(name.to_string(), self.g_symbol_table.len(), false, Type::Void));
            self.g_symbol_table.push(Rc::clone(&obj));
            self.current_function = Some(Function::new(name));
            self.idx += 1;
            self.expect(TokenKind::LParen);
            while !self.eat(TokenKind::RParen) {
                if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
                    self.idx += 1;
                    if self.current_function.as_mut().unwrap().param_symbol_table.find(name).is_some() {
                        panic!("A local variable or function named '{}' is already defined in this scope", name);
                    } else {
                        self.expect(TokenKind::Colon);
                        let typekind = self.type_no_bounds();
                        let current_function = self.current_function.as_mut().unwrap();
                        let obj = Rc::new(Object::new(name.to_string(), current_function.param_symbol_table.len(), true, typekind.clone()));
                        current_function.param_symbol_table.push(Rc::clone(&obj));
                    }
                } else {
                    panic!("expected identifier");
                }
                self.eat(TokenKind::Comma);
            }
        } else {
            eprintln!("{}^", " ".repeat(self.tokens[self.idx].cur));
            panic!("expected identifier");
        }

        if self.eat(TokenKind::RArrow) {
            if let TokenKind::Type(typekind) = &self.tokens[self.idx].kind {
                self.idx += 1;
                self.current_function.as_mut().unwrap().rettype = typekind.clone();
            } else {
                eprintln!("{}^", " ".repeat(self.tokens[self.idx].cur));
                panic!("expected type, but got `{:?}`", self.tokens[self.idx].kind);
            }
        }

        self.current_function.as_mut().unwrap().statements = self.block_expression();
        self.current_function.take().unwrap()
    }

    fn statement(&mut self) -> Node {
        let node =  if self.eat_keyword(Keyword::Return) {
            if self.eat(TokenKind::Semi) {
                return new_return_node(None);
            }
            new_return_node(Some(self.expr()))
        } else if self.eat_keyword(Keyword::Let) {
            if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
                self.idx += 1;
                self.expect(TokenKind::Colon);
                let typekind = self.type_no_bounds();
                let node = new_variable_node_with_let(&mut self.current_function.as_mut().unwrap().lvar_symbol_table, name, typekind);
                if self.eat(TokenKind::Assign) {
                    let node = new_assign_node(node, self.expr());
                    self.expect(TokenKind::Semi);
                    node
                } else {
                    self.expect(TokenKind::Semi);
                    self.statement()
                }
            } else {
                panic!("The left-hand side of an assignment must be a variable")
            }
        } else {
            let mut node = self.expr();
            while self.eat(TokenKind::Dot) {
                if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
                    self.idx += 1;
                    node = new_field_node(self.current_function.as_mut().unwrap(), name, node);
                } else {
                    panic!("unexpected token: `{:?}`", self.tokens[self.idx].kind);
                }
            }
            node
        };

        if self.eat(TokenKind::Semi) {
            node
        } else {
            new_evaluates_node(node)
        }
    }

    fn type_no_bounds(&mut self) -> Type {
        if self.eat(TokenKind::And) {
            Type::Ptr(Box::new(self.type_no_bounds()))
        } else if self.eat(TokenKind::AndAnd) {
            Type::Ptr(Box::new(Type::Ptr(Box::new(self.type_no_bounds()))))
        } else if let TokenKind::Type(typekind) = &self.tokens[self.idx].kind {
            self.idx += 1;
            typekind.clone()
        } else if let TokenKind::Ident(name) = &self.tokens[self.idx].kind {
            self.idx += 1;
            Type::Struct(name.to_string())
        } else {
            panic!("expected type, but got `{:?}`", self.tokens[self.idx].kind);
        }

    }

    fn block_expression(&mut self) -> Node {
        let mut stmts = vec![];
        self.current_function.as_mut().unwrap().lvar_symbol_table.enter_scope();
        self.expect(TokenKind::LBrace );
        let except_struct_expression = self.except_struct_expression;
        self.except_struct_expression = false;
        while !self.eat(TokenKind::RBrace) && !self.is_eof() {
            stmts.push(self.statement());
        }
        self.except_struct_expression = except_struct_expression;
        self.current_function.as_mut().unwrap().lvar_symbol_table.leave_scope();
        new_block_node(stmts)
    }

    fn expr(&mut self) -> Node {
        if self.tokens[self.idx].kind == TokenKind::LBrace {
            self.block_expression()
        } else if self.eat_keyword(Keyword::If) {
            self.except_struct_expression = true;
            let cond = self.expr();
            self.except_struct_expression = false;
            let then = self.block_expression();
            let els = if self.eat_keyword(Keyword::Else) {
                Some(self.block_expression())
            } else {
                None
            };
            new_if_node(cond, then, els)
        } else if self.eat_keyword(Keyword::While) {
            let cond = self.expr();
            let then = self.block_expression();
            new_while_node(cond, then)
        } else if self.eat_keyword(Keyword::Loop) {
            new_loop_node(self.block_expression())
        } else {
            self.assign()
        }
    }

    fn assign(&mut self) -> Node {
        let node = self.logical_or();

        if self.eat(TokenKind::Assign) {
            new_assign_node(node, self.expr())
        } else if self.eat(TokenKind::PlusEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Add, node, self.expr()))
        } else if self.eat(TokenKind::MinusEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Sub, node, self.expr()))
        } else if self.eat(TokenKind::StarEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Mul, node, self.expr()))
        } else if self.eat(TokenKind::SlashEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Div, node, self.expr()))
        } else if self.eat(TokenKind::PercentEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Rem, node, self.expr()))
        } else if self.eat(TokenKind::AndEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitAnd, node, self.expr()))
        } else if self.eat(TokenKind::CaretEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitXor, node, self.expr()))
        } else if self.eat(TokenKind::OrEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::BitOr, node, self.expr()))
        } else if self.eat(TokenKind::ShlEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Shl, node, self.expr()))
        } else if self.eat(TokenKind::ShrEq) {
            let lhs = node.clone();
            new_assign_node(lhs, new_binary_op_node(BinaryOpKind::Shr, node, self.expr()))
        } else {
            node
        }
    }

    fn logical_or(&mut self) -> Node {
        let mut node = self.logical_and();

        loop {
            if self.eat(TokenKind::OrOr) {
                node = new_short_circuit_op_node(ShortCircuitOpKind::Or, node, self.logical_and());
            } else {
                return node;
            }
        }
    }

    fn logical_and(&mut self) -> Node {
        let mut node = self.equality();

        loop {
            if self.eat(TokenKind::AndAnd) {
                node = new_short_circuit_op_node(ShortCircuitOpKind::And, node, self.equality());
            } else {
                return node;
            }
        }
    }

    fn equality(&mut self) -> Node {
        let mut node = self.relational();

        loop {
            if self.eat(TokenKind::EqEq) {
                node = new_binary_op_node(BinaryOpKind::Eq, node, self.relational());
            } else if self.eat(TokenKind::Ne) {
                node = new_binary_op_node(BinaryOpKind::Ne, node, self.relational());
            } else {
                return node;
            }
        }
    }

    fn relational(&mut self) -> Node {
        let mut node = self.bitor();

        loop {
            if self.eat(TokenKind::Lt) {
                node = new_binary_op_node(BinaryOpKind::Lt, node, self.bitor());
            } else if self.eat(TokenKind::Le) {
                node = new_binary_op_node(BinaryOpKind::Le, node, self.bitor());
            } else if self.eat(TokenKind::Gt) {
                node = new_binary_op_node(BinaryOpKind::Gt, node, self.bitor());
            } else if self.eat(TokenKind::Ge) {
                node = new_binary_op_node(BinaryOpKind::Ge, node, self.bitor());
            } else {
                return node;
            }
        }
    }

    fn bitor(&mut self) -> Node {
        let mut node = self.bitxor();
        while self.eat(TokenKind::Or) {
            node = new_binary_op_node(BinaryOpKind::BitOr, node, self.bitxor());
        }
        node
    }

    fn bitxor(&mut self) -> Node {
        let mut node = self.bitand();
        while self.eat(TokenKind::Caret) {
            node = new_binary_op_node(BinaryOpKind::BitXor, node, self.bitand());
        }
        node
    }

    fn bitand(&mut self) -> Node {
        let mut node = self.shift();
        while self.eat(TokenKind::And) {
            node = new_binary_op_node(BinaryOpKind::BitAnd, node, self.shift());
        }
        node
    }

    fn shift(&mut self) -> Node {
        let mut node = self.add();

        loop {
            if self.eat(TokenKind::Shl) {
                node = new_binary_op_node(BinaryOpKind::Shl, node, self.add());
            } else if self.eat(TokenKind::Shr) {
                node = new_binary_op_node(BinaryOpKind::Shr, node, self.add());
            } else {
                return node;
            }
        }
    }

    fn add(&mut self) -> Node {
        let mut node = self.mul();

        loop {
            if self.eat(TokenKind::Plus) {
                node = new_binary_op_node(BinaryOpKind::Add, node, self.mul());
            } else if self.eat(TokenKind::Minus) {
                node = new_binary_op_node(BinaryOpKind::Sub, node, self.mul());
            } else {
                return node;
            }
        }
    }

    fn mul(&mut self) -> Node {
        let mut node = self.cast();

        loop {
            if self.eat(TokenKind::Star) {
                node = new_binary_op_node(BinaryOpKind::Mul, node, self.cast());
            } else if self.eat(TokenKind::Slash) {
                node = new_binary_op_node(BinaryOpKind::Div, node, self.cast());
            } else if self.eat(TokenKind::Percent) {
                node = new_binary_op_node(BinaryOpKind::Rem, node, self.cast());
            } else {
                return node;
            }
        }
    }

    fn cast(&mut self) -> Node {
        let mut node = self.unary();

        loop {
            if self.eat_keyword(Keyword::As) {
                let typekind = self.type_no_bounds();
                node = new_cast_node(typekind, node);
            } else {
                return node;
            }
        }
    }

    fn unary(&mut self) -> Node {
        if self.eat(TokenKind::Minus) {
            new_unary_op_node(UnaryOpKind::Neg, self.unary())
        } else if self.eat(TokenKind::Not) {
            new_unary_op_node(UnaryOpKind::Not, self.unary())
        } else if self.eat(TokenKind::And) {
            new_unary_op_node(UnaryOpKind::Ref, self.unary())
        } else if self.eat(TokenKind::Star) {
            new_unary_op_node(UnaryOpKind::Deref, self.unary())
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Node {
        if self.eat(TokenKind::LParen) {
            let except_struct_expression = self.except_struct_expression;
            self.except_struct_expression = false;
            let node = self.expr();
            self.expect(TokenKind::RParen);
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
                        args.push(self.expr());
                        self.eat(TokenKind::Comma);
                    }
                    new_function_call_node(name, args)
                } else if !self.except_struct_expression && self.eat(TokenKind::LBrace) {
                    // struct
                    let mut field = vec![];
                    while !self.eat(TokenKind::RBrace) {
                        field.push(self.expr());
                        self.eat(TokenKind::Comma);
                    }
                    new_struct_expr_node(&mut self.current_function.as_mut().unwrap().lvar_symbol_table, name, field)
                } else {
                    // local variable or parameter
                    new_variable_node(self.current_function.as_mut().unwrap(), name)
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
                    args.push(self.expr());
                    self.eat(TokenKind::Comma);
                }
                new_builtin_call_node(*kind, args)
            }
            _ => {
                eprintln!("{}^", " ".repeat(self.tokens[self.idx].cur));
                panic!("illegal TokenKind `{:?}`", self.tokens[self.idx].kind);
            }
        }
    }
}
