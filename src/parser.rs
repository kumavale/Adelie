use crate::ast::*;
use crate::builtin::*;
use crate::class::{ClassKind, Class, Impl, EnumDef};
use crate::error::*;
use crate::function::Function;
use crate::keyword::{Type, RRType, Keyword};
use crate::object::{Object, ObjectKind, FindSymbol, SymbolTable};
use crate::program::Program;
use crate::token::{Delimiter, Token, TokenKind, LiteralKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Grammar
//
// Program :
//     ItemWithAttrs *
//
// ItemWithAttrs :
//     OuterAttr * Item
// OuterAttr :
//     `#` `[` MetaItem `]`
// MetaItem :
//     IDENTIFIER
//   | IDENTIFIER ( IDENTIFIER `=` LITERAL )
//   | IDENTIFIER `=` LITERAL
//
// Item :
//     Function
//   | Mod
//   | ForeignMod
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
// ForeignMod :
//     `extern` `{` ForeignItem * `}`
// ForeignItem :
//     Function
//   | Struct
//   | Implementation
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
//   | LambdaExpression
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
// LambdaExpression :
//     `||` FunctionReturnType ? Expression
//   | (`|` FunctionParam ( `,` FunctionParam ) * `|`) FunctionReturnType ? Expression
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
// LITERAL :
//     CHAR_LITERAL
//   | STRING_LITERAL
//   | INTEGER_LITERAL
//   | BOOLEAN_LITERAL
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
//         | Box
//

#[derive(Debug)]
pub struct Parser<'a> {
    path: &'a str,
    input: &'a str,
    lines: Vec<&'a str>,
    g_symbol_table: &'a mut SymbolTable,  // global symbol table
    current_fn: Option<Function<'a>>,
    current_lambda: Option<Function<'a>>,
    current_impl: Option<Impl<'a>>,
    current_class: Vec<String>,
    tokens: &'a [Token],
    idx: usize,
    except_struct_expression: bool,
    brk_label_seq: usize,
    loop_count: usize,
    current_mod: Vec<String>,
    errors: Rc<RefCell<Errors>>,
    is_foreign: bool,
    foreign_reference: Option<String>,
    ident_types: HashMap<(Vec<String>, String), RRType>,  // (path, name)
    nested_class_instance: Option<Node<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(
        path: &'a str,
        input: &'a str,
        tokens: &'a [Token],
        g_symbol_table: &'a mut SymbolTable,
        errors: Rc<RefCell<Errors>>,
    ) -> Self {
        Parser {
            path,
            input,
            lines: input.lines().collect(),
            g_symbol_table,
            current_fn: None,
            current_lambda: None,
            current_impl: None,
            current_class: vec![],
            tokens,
            idx: 0,
            except_struct_expression: false,
            brk_label_seq: 0,
            loop_count: 0,
            current_mod: vec![],
            errors,
            is_foreign: false,
            foreign_reference: None,
            ident_types: HashMap::new(),
            nested_class_instance: None,
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
        } else if !self.is_eof() {
            e0001(Rc::clone(&self.errors), self.errorset(), kind);
            self.idx += 1;
        }
    }

    fn expect_ident(&mut self) -> String {
        if let Some(ident) = self.eat_ident() {
            ident
        } else {
            e0003(Rc::clone(&self.errors), self.errorset());
            "".to_string()
        }
    }

    fn expect_string_literal(&mut self) -> String {
        if let TokenKind::Literal(LiteralKind::String(s)) = &self.tokens[self.idx].kind {
            self.idx += 1;
            s.to_string()
        } else {
            let message = format!("expect string literal, but got {}", self.tokens[self.idx].kind);
            e0000(Rc::clone(&self.errors), self.errorset(), &message);
            self.idx += 1;
            "".to_string()
        }
    }

    fn bump(&mut self) {
        self.idx += 1;
    }

    fn close_delimiter(&mut self, delim: Delimiter, start_brace: Token) {
        let mut brace_count = 1;
        while !self.is_eof() {
            match &self.tokens[self.idx].kind {
                d if *d == TokenKind::CloseDelim(delim) => {
                    brace_count -= 1;
                    if brace_count == 0 {
                        self.idx += 1;
                        return;
                    }
                }
                d if *d == TokenKind::OpenDelim(delim) => brace_count += 1,
                _ => (),  // Do nothing
            }
            self.idx += 1;
        }
        e0000(Rc::clone(&self.errors), (self.path, &self.lines, &[start_brace]),
            "this file contains an unclosed delimiter");
    }

    fn is_eof(&self) -> bool {
        self.tokens[self.idx].kind == TokenKind::Eof
    }

    fn current_token(&self) -> &[Token] {
        &self.tokens[self.idx..=self.idx]
    }

    fn current_fn(&self) -> &Function<'a> {
        if let Some(local_fn) = self.current_lambda.as_ref() {
            local_fn
        } else {
            self.current_fn.as_ref().unwrap()
        }
    }

    fn current_fn_mut(&mut self) -> &mut Function<'a> {
        if let Some(local_fn) = self.current_lambda.as_mut() {
            local_fn
        } else {
            self.current_fn.as_mut().unwrap()
        }
    }

    fn type_no_bounds(&mut self) -> Option<RRType> {
        if self.eat(TokenKind::And) {
            Some(RRType::new(Type::Ptr(self.type_no_bounds()?)))
        } else if self.eat(TokenKind::AndAnd) {
            Some(RRType::new(Type::Ptr(RRType::new(Type::Ptr(self.type_no_bounds()?)))))
        } else if let TokenKind::Type(ty) = &self.tokens[self.idx].kind {
            self.idx += 1;
            Some(RRType::new(ty.clone()))
        } else if self.eat_keyword(Keyword::Box) {
            self.expect(TokenKind::Lt);
            let ty = self.type_no_bounds()?;
            self.expect(TokenKind::Gt);
            Some(RRType::new(Type::Box(ty)))
        } else if let Some(ident) = self.eat_ident() {
            // enum, struct, class のいずれかになる
            // この時点ではそれが何かは定かではないので、識別子を示す特殊なTypeを返す
            // AST構築後に変換する
            // TODO: self.ident_typesにType::RRIdentが無いか検査
            let (path, name) = if self.check(TokenKind::PathSep) {
                let mut path = vec![ident];
                while self.eat(TokenKind::PathSep) {
                    path.push(self.expect_ident());
                }
                let ident = path.pop().unwrap();
                (path, ident)
            } else {
                (self.current_mod.to_vec(), ident)
            };
            if let Some(ty) = self.ident_types.get(&(path.to_vec(), name.to_string())) {
                // known ident
                Some(RRType::clone(ty))
            } else {
                // insert
                let tmp_ty = RRType::new(Type::RRIdent(path.to_vec(), name.to_string()));
                self.ident_types.insert((path, name), RRType::clone(&tmp_ty));
                Some(tmp_ty)
            }
        } else if self.eat_keyword(Keyword::SelfUpper) {
            if self.current_impl.is_none() {
                let message = "`Self` is only available in impls";
                e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), message);
                None
            } else {
                let org_name = self.current_impl.as_ref().unwrap().name.to_string();
                if let Some(ty) = self.ident_types.get(&(self.current_mod.to_vec(), org_name.to_string())) {
                    // known ident
                    Some(RRType::clone(ty))
                } else {
                    // insert
                    let tmp_ty = RRType::new(Type::RRIdent(self.current_mod.to_vec(), org_name.to_string()));
                    self.ident_types.insert((self.current_mod.to_vec(), org_name), RRType::clone(&tmp_ty));
                    Some(tmp_ty)
                }
            }
        } else {
            e0002(Rc::clone(&self.errors), self.errorset());
            None
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
        let mut program = Program::new(self.path, self.input, Rc::clone(&self.errors));
        self.current_class.push(program.name.to_string());
        while let Some(item) = self.parse_item_with_attrs() {
            self.parse_program(&mut program, item.0, item.1);
        }
        program
    }

    fn parse_program(&mut self, program: &mut Program<'a>, begin: usize, item: Item<'a>) {
        match item.kind {
            ItemKind::Class(cl) => {
                for nested_class in &cl.nested_class {
                    if program.current_namespace.borrow().find_class(|k|matches!(k,ClassKind::NestedClass(_)), &nested_class.name).is_some() {
                        e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[begin..=begin+1]), &nested_class.name);
                    }
                    program.current_namespace.borrow_mut().push_class(nested_class.clone());
                }
                if program.current_namespace.borrow().find_class(|k|matches!(k,ClassKind::Struct|ClassKind::Class), &cl.name).is_some() {
                    e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[begin..=begin+1]), &cl.name);
                }
                program.current_namespace.borrow_mut().push_class(cl);
            }
            ItemKind::Enum(ed) => {
                if program.current_namespace.borrow().find_enum(&ed.name).is_some() {
                    e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[begin..=begin+1]), &ed.name);
                }
                program.current_namespace.borrow_mut().push_enum(ed);
            }
            ItemKind::Impl(impl_item) => {
                if let Some(cl) = program.current_namespace.borrow().find_class(|_|true, &impl_item.name) {
                    cl.borrow_mut().impls.push(Rc::new(impl_item));
                } else {
                    // TODO: クラス定義より先にimplしても大丈夫なように
                    e0000(Rc::clone(&self.errors), self.errorset(), "TODO");
                }
            }
            ItemKind::Mod(mod_item) => {
                program.enter_namespace(&mod_item.0);
                program.current_namespace.borrow_mut().is_foreign = self.is_foreign;
                for item in mod_item.1 {
                    self.parse_program(program, item.0, item.1);
                }
                program.leave_namespace();
            }
            ItemKind::ForeignMod(foreign_mod_item) => {
                if self.foreign_reference.is_none() {
                    let message = "specify the `.dll` file";
                    e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[begin..=begin+1]), message);
                }
                self.foreign_reference = None;
                program.references.extend_from_slice(&item.attrs.iter()
                    .filter(|attr| attr.find_item("link").is_some())
                    .cloned()
                    .collect::<Vec<Attribute>>());
                program.enter_namespace("extern");
                program.current_namespace.borrow_mut().is_foreign = true;
                foreign_mod_item.into_iter().for_each(|item| match item {
                        ForeignItemKind::Fn(f)     => program.current_namespace.borrow_mut().push_fn(f),
                        ForeignItemKind::Enum(e)   => program.current_namespace.borrow_mut().push_enum(e),
                        ForeignItemKind::Class(c)  => program.current_namespace.borrow_mut().push_class(c),
                        ForeignItemKind::Impl(i)   => {
                            if let Some(cl) = program.current_namespace.borrow().find_class(|_|true, &i.name) {
                                cl.borrow_mut().impls.push(Rc::new(i));
                            } else {
                                // TODO: クラス定義より先にimplしても大丈夫なように
                                e0000(Rc::clone(&self.errors), self.errorset(), "TODO");
                            }
                        }
                        ForeignItemKind::Mod((ident, items)) => {
                            program.enter_namespace(&ident);
                            program.current_namespace.borrow_mut().is_foreign = true;
                            for item in items {
                                self.parse_program(program, item.0, item.1);
                            }
                            program.leave_namespace();
                        }
                });
                self.is_foreign = false;
                program.leave_namespace();
            }
            ItemKind::Fn(f) => {
                if program.current_namespace.borrow().find_fn(&f.name).is_some() {
                    e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[begin..=begin+1]), &f.name);
                }
                program.current_namespace.borrow_mut().push_fn(f);
            }
        }
    }

    fn parse_item_with_attrs(&mut self) -> Option<(usize, Item<'a>)> {
        let mut attrs = vec![];
        while self.eat(TokenKind::Pound) {
            attrs.push(self.parse_outer_attr());
        }
        let begin = self.idx;
        let kind = if self.eat_keyword(Keyword::Struct) {
            let st = self.parse_item_class(ClassKind::Struct);
            ItemKind::Class(st)
        } else if self.eat_keyword(Keyword::Class) {
            let class = self.parse_item_class(ClassKind::Class);
            ItemKind::Class(class)
        } else if self.eat_keyword(Keyword::Enum) {
            let enum_item = self.parse_item_enum();
            ItemKind::Enum(enum_item)
        } else if self.eat_keyword(Keyword::Impl) {
            let impl_item = self.parse_item_impl();
            ItemKind::Impl(impl_item)
        } else if self.eat_keyword(Keyword::Mod) {
            let mod_item = self.parse_item_mod();
            ItemKind::Mod(mod_item)
        } else if self.eat_keyword(Keyword::Extern) {
            self.foreign_reference = attrs.iter()
                .find(|attr| attr.find_item("link").is_some())
                .and_then(|attr| attr.find_value("name"))
                .and_then(|name| if name.ends_with(".dll") { Some(name) } else { None })
                .and_then(|name| name.get(..name.len()-4))
                .map(|name| name.to_string());
            let foreign_mod_item = self.parse_item_foreign_mod();
            ItemKind::ForeignMod(foreign_mod_item)
        } else if self.eat_keyword(Keyword::Fn) {
            let f = self.parse_item_fn();
            ItemKind::Fn(f)
        } else if self.is_eof() || self.check(TokenKind::CloseDelim(Delimiter::Brace)) {
            return None;
        } else {
            e0004(Rc::clone(&self.errors), self.errorset());
            return None;
        };
        Some((begin, Item { attrs, kind, }))
    }

    fn parse_outer_attr(&mut self) -> Attribute {
        // WIP: とりあえず `#[attr(foo="bar",foo="bar",...)]` だけパースする
        let mut kvs = vec![];
        self.expect(TokenKind::OpenDelim(Delimiter::Bracket));
        let ident = self.expect_ident();
        self.expect(TokenKind::OpenDelim(Delimiter::Parenthesis));
        let key = self.expect_ident();
        self.expect(TokenKind::Assign);
        let value = self.expect_string_literal();
        kvs.push((key, value));
        while self.eat(TokenKind::Comma) {
            let key = self.expect_ident();
            self.expect(TokenKind::Assign);
            let value = self.expect_string_literal();
            kvs.push((key, value));
        }
        self.expect(TokenKind::CloseDelim(Delimiter::Parenthesis));
        self.expect(TokenKind::CloseDelim(Delimiter::Bracket));
        Attribute { item: AttrItem::Delimited(ident, kvs) }
    }

    fn parse_item_mod(&mut self) -> (String, Vec<(usize, Item<'a>)>) {
        let id = self.expect_ident();
        self.current_mod.push(id.to_string());
        let mod_kind = if self.eat(TokenKind::Semi) {
            // TODO
            todo!("ModKind::Unloaded");
        } else {
            self.expect(TokenKind::OpenDelim(Delimiter::Brace));
            self.parse_mod()
        };
        self.current_mod.pop();
        (id, mod_kind)
    }

    fn parse_mod(&mut self) -> Vec<(usize, Item<'a>)> {
        let mut items = vec![];
        while let Some(item) = self.parse_item_with_attrs() {
            items.push(item);
        }
        self.expect(TokenKind::CloseDelim(Delimiter::Brace));
        items
    }

    fn parse_item_foreign_mod(&mut self) -> Vec<ForeignItemKind<'a>> {
        self.is_foreign = true;
        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
        let mut foreign_items = vec![];
        while let Some(item) = self.parse_item_with_attrs() {
            let item = match item.1.kind {
                ItemKind::Fn(f)     => ForeignItemKind::Fn(f),
                ItemKind::Mod(m)    => ForeignItemKind::Mod(m),
                ItemKind::Class(c)  => ForeignItemKind::Class(c),
                ItemKind::Impl(i)   => ForeignItemKind::Impl(i),
                ItemKind::Enum(e)   => ForeignItemKind::Enum(e),
                _ => {
                    let message = "not supported in `extern` blocks";
                    e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[item.0..=item.0]), message);
                    continue;
                }
            };
            foreign_items.push(item);
        }
        self.expect(TokenKind::CloseDelim(Delimiter::Brace));
        foreign_items
    }

    fn parse_item_enum(&mut self) -> EnumDef {
        let name = self.expect_ident();
        let ed = EnumDef::new(name, self.current_mod.to_vec());
        if let Some(ty) = self.ident_types.get_mut(&(self.current_mod.to_vec(), ed.name.to_string())) {
            // replace
            let (path, name) = if let Type::RRIdent(path, name) = &*ty.borrow() {
                (path.to_vec(), name.to_string())
            } else {
                unreachable!();
            };
            *ty.borrow_mut() = Type::Enum(self.foreign_reference.clone(), path, name);
        } else {
            // insert
            let ty = RRType::new(Type::Enum(self.foreign_reference.clone(), self.current_mod.to_vec(), ed.name.to_string()));
            self.ident_types.insert((self.current_mod.to_vec(), ed.name.to_string()), ty);
        }
        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
        //let start_brace = self.idx-1;
        while !self.eat(TokenKind::CloseDelim(Delimiter::Brace)) {
            todo!();
            // ↓parse_item_structを引用
            //let ident = self.expect_ident();
            //if ident.is_empty() {
            //    self.close_delimiter(Delimiter::Brace, self.tokens[start_brace].clone());
            //    break;
            //}
            //if st.field.iter().any(|o|o.name==ident) {
            //    e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), &ident);
            //}
            //self.expect(TokenKind::Colon);
            //if let Some(ty) = self.type_no_bounds() {
            //    let obj = Object::new(ident, st.field.len(), false, ty, false);
            //    st.field.push(obj);
            //}
            //if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Brace)) {
            //    e0008(Rc::clone(&self.errors), self.errorset());
            //}
        }
        ed
    }

    fn parse_item_class(&mut self, kind: ClassKind) -> Class<'a> {
        if !self.is_foreign && matches!(kind, ClassKind::Class | ClassKind::NestedClass(_)) {
            e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..=self.idx]), "`class` must be inside an extern block");
        }
        let name = self.expect_ident();
        self.current_class.push(name.to_string());
        let mut cl = Class::new(kind.clone(), name, self.current_mod.to_vec(), self.foreign_reference.clone());

        if self.eat(TokenKind::Colon) {
            // クラスの継承
            let base_class = self.type_no_bounds();
            cl.base = base_class;
        }

        if let Some(ty) = self.ident_types.get_mut(&(self.current_mod.to_vec(), cl.name.to_string())) {
            // replace
            let (path, name) = if let Type::RRIdent(path, name) = &*ty.borrow() {
                (path.to_vec(), name.to_string())
            } else {
                unreachable!();
            };
            *ty.borrow_mut() = Type::Class(kind, self.foreign_reference.clone(), path, name, cl.base.clone(), false);
        } else {
            // insert
            let ty = RRType::new(Type::Class(kind, self.foreign_reference.clone(), self.current_mod.to_vec(), cl.name.to_string(), cl.base.clone(), false));
            self.ident_types.insert((self.current_mod.to_vec(), cl.name.to_string()), ty);
        }

        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
        let start_brace = self.idx-1;
        while !self.eat(TokenKind::CloseDelim(Delimiter::Brace)) {
            if self.eat_keyword(Keyword::Class) {
                // nested class
                let class = self.parse_item_class(ClassKind::NestedClass(cl.name.to_string()));
                cl.nested_class.push(class);
                self.eat(TokenKind::Comma);
            } else if self.eat_keyword(Keyword::Impl) {
                // impl of nested class
                let impl_item = self.parse_item_impl();
                if let Some(cl) = cl.nested_class.find_mut(&impl_item.name) {
                    cl.impls.push(Rc::new(impl_item));
                } else {
                    // TODO: クラス定義より先にimplしても大丈夫なように
                    e0000(Rc::clone(&self.errors), self.errorset(), "TODO");
                }
                self.eat(TokenKind::Comma);
            } else {
                // field or property
                let ident = self.expect_ident();
                if ident.is_empty() {
                    self.close_delimiter(Delimiter::Brace, self.tokens[start_brace].clone());
                    break;
                }

                self.expect(TokenKind::Colon);
                let ty = self.type_no_bounds();

                if self.eat(TokenKind::OpenDelim(Delimiter::Brace)) {
                    // property
                    if cl.properties.iter().any(|o|o.name==ident) {
                        e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), &ident);
                    }
                    // TODO
                    if self.eat_keyword(Keyword::Set) {
                        self.expect(TokenKind::Semi);
                        if self.eat_keyword(Keyword::Get) {
                            self.expect(TokenKind::Semi);
                        }
                    } else if self.eat_keyword(Keyword::Get) {
                        self.expect(TokenKind::Semi);
                        if self.eat_keyword(Keyword::Set) {
                            self.expect(TokenKind::Semi);
                        }
                    } else {
                    }
                    // TODO
                    if let Some(ident) = self.eat_ident() {
                        if ident == "add" {
                            self.expect(TokenKind::Semi);
                        }
                    }
                    if let Some(ty) = ty {
                        // TODO: backing fieldの生成
                        let obj = Object::new(ident, cl.field.offset(ObjectKind::Field), ObjectKind::Local, ty.clone(), false);
                        cl.properties.push(obj);
                        let dummy = Object::new("".to_string(), cl.field.offset(ObjectKind::Field), ObjectKind::Local, ty, false);
                        cl.field.push(Rc::new(RefCell::new(dummy)));
                    }
                    self.expect(TokenKind::CloseDelim(Delimiter::Brace));
                    self.eat(TokenKind::Comma);
                } else {
                    // field
                    if cl.field.find(&ident).is_some() {
                        e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), &ident);
                    }
                    if let Some(ty) = ty {
                        let obj = Object::new(ident, cl.field.offset(ObjectKind::Field), ObjectKind::Field, ty, false);
                        cl.field.push(Rc::new(RefCell::new(obj)));
                    }
                    if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Brace)) {
                        e0008(Rc::clone(&self.errors), self.errorset());
                    }
                }
            }
        }
        self.current_class.pop();
        cl
    }

    fn parse_item_impl(&mut self) -> Impl<'a> {
        let name= self.expect_ident();
        self.current_impl = Some(Impl::new(name, self.current_mod.to_vec(), self.foreign_reference.clone()));
        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
        while self.eat_keyword(Keyword::Fn) {
            let func = self.parse_item_fn();
            self.current_impl.as_mut().unwrap()
                .functions
                .push(Rc::new(func));
        }
        self.expect(TokenKind::CloseDelim(Delimiter::Brace));
        self.current_impl.take().unwrap()
    }

    fn parse_item_fn(&mut self) -> Function<'a> {
        let (ident, is_ctor) = if self.eat_keyword(Keyword::Ctor) {
            (".ctor".to_string(), true)
        } else {
            (self.expect_ident(), false)
        };
        let obj = Rc::new(RefCell::new(Object::new(ident.to_string(), 0, ObjectKind::Local, RRType::new(Type::Void), false)));
        self.g_symbol_table.push(Rc::clone(&obj));
        self.current_fn = Some(Function::new(&ident, is_ctor));

        if is_ctor {
            if !self.is_foreign {
                e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), "`.ctor` must be inside an extern block");
            } else if let Some(ref im) = self.current_impl {
                let ty = self.ident_types.get(&(self.current_mod.to_vec(), im.name.to_string())).unwrap();
                self.current_fn_mut().rettype = RRType::clone(ty);
            } else {
                e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), "`.ctor` must be an impl");
            }
        }

        self.expect(TokenKind::OpenDelim(Delimiter::Parenthesis));
        let start_brace = self.idx-1;
        if !self.eat(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
            // TODO: 所有権の実装後に`&`なしの`self`に対応
            if self.eat(TokenKind::And) {
                let is_mutable = self.eat_keyword(Keyword::Mut);
                if self.eat_keyword(Keyword::SelfLower) {
                    // (&self) -> (self: &Self)
                    self.current_fn_mut().is_static = false;
                    let ident = "self".to_string();
                    let ty = RRType::new(Type::_Self(self.current_mod.to_vec(), self.current_impl.as_ref().unwrap().name.to_string(), false));
                    let obj = Rc::new(RefCell::new(Object::new(ident, self.current_fn().symbol_table.borrow().offset(ObjectKind::Param), ObjectKind::Param, ty, is_mutable)));
                    obj.borrow_mut().assigned = true;
                    self.current_fn_mut().symbol_table.borrow_mut().push(Rc::clone(&obj));
                    if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
                        e0009(Rc::clone(&self.errors), self.errorset());
                    }
                } else {
                    e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..=self.idx]),
                        "variable declaration cannot be a reference");
                }
            }
            while !self.eat(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
                self.parse_fn_params();
                if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
                    e0009(Rc::clone(&self.errors), self.errorset());
                    self.close_delimiter(Delimiter::Parenthesis, self.tokens[start_brace].clone());
                    break;
                }
            }
        }

        if is_ctor && self.check(TokenKind::RArrow) {
            e0000(Rc::clone(&self.errors), self.errorset(), "`.ctor` cannot have a return type");
        }

        if self.eat(TokenKind::RArrow) {
            self.parse_fn_ret_ty();
        }

        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
        self.current_fn_mut().statements = self.parse_block_expr();
        self.current_fn.take().unwrap()
    }

    fn parse_fn_params(&mut self) {
        if self.eat(TokenKind::And) {
            e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..=self.idx]),
                "variable declaration cannot be a reference");
        }
        let is_mutable = self.eat_keyword(Keyword::Mut);
        let ident = self.expect_ident();
        if ident.is_empty() {
            self.bump();
            return;
        }
        if self.current_fn_mut().symbol_table.borrow().find(&ident).is_some() {
            e0005(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), &ident);
        }
        self.expect(TokenKind::Colon);
        if let Some(ty) = self.type_no_bounds() {
            let obj = Rc::new(RefCell::new(Object::new(ident, self.current_fn().symbol_table.borrow().offset(ObjectKind::Param), ObjectKind::Param, ty, is_mutable)));
            obj.borrow_mut().assigned = true;
            self.current_fn_mut().symbol_table.borrow_mut().push(Rc::clone(&obj));
        }
    }

    fn parse_fn_ret_ty(&mut self) {
        if let Some(ty) = self.type_no_bounds() {
            self.current_fn_mut().rettype = ty;
        }
    }

    fn parse_block_expr(&mut self) -> Node<'a> {
        self.current_fn_mut().symbol_table.borrow_mut().enter_scope();
        let except_struct_expression = self.except_struct_expression;
        self.except_struct_expression = false;
        let begin = self.idx;
        let mut stmts = if self.check(TokenKind::CloseDelim(Delimiter::Brace)) {
            vec![new_empty_node()]
        } else {
            vec![]
        };
        while !self.check(TokenKind::CloseDelim(Delimiter::Brace)) && !self.is_eof() {
            stmts.push(self.parse_stmt());
        }
        self.close_delimiter(Delimiter::Brace, self.tokens[begin-1].clone());
        self.except_struct_expression = except_struct_expression;
        self.current_fn_mut().symbol_table.borrow_mut().leave_scope();
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
            e0000(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]),
                "cannot `break` outside of a loop");
            return new_empty_node();
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
        let ty = self.type_no_bounds().unwrap_or_else(|| RRType::new(Type::Void));
        let token = &self.tokens[begin..self.idx];
        let node = new_variable_node_with_let(
            &mut self.current_fn_mut().symbol_table.borrow_mut(),
            ident,
            ty,
            token,
            is_mutable,
            false,
            ObjectKind::Local,
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
        if self.eat(TokenKind::OpenDelim(Delimiter::Brace)) {
            self.parse_block_expr()
        } else if self.eat_keyword(Keyword::If) {
            self.parse_if_expr()
        } else if self.eat_keyword(Keyword::Loop) {
            self.parse_infinite_loop_expr()
        } else if self.eat_keyword(Keyword::While) {
            self.parse_predicate_loop_expr()
        } else if self.eat(TokenKind::OrOr) {
            // WIP: とりあえず引数なしのクロージャのみ実装
            self.parse_lambda_expr(None)
        } else {
            self.parse_assign()
        }
    }

    fn parse_if_expr(&mut self) -> Node<'a> {
        let begin = self.idx - 1;
        let cond = self.parse_cond();
        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
        let then = self.parse_block_expr();
        let els = if self.eat_keyword(Keyword::Else) {
            self.expect(TokenKind::OpenDelim(Delimiter::Brace));
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
        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
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
        self.expect(TokenKind::OpenDelim(Delimiter::Brace));
        let then = self.parse_block_expr();
        self.loop_count -= 1;
        self.brk_label_seq = tmp;
        new_while_node(cond, then, brk_label_seq, &self.tokens[begin..self.idx])
    }

    // WIP
    fn parse_lambda_expr(&mut self, _args: Option<Rc<RefCell<Object>>>) -> Node<'a> {
        // とりあえず引数も戻り値もなし
        // とりあえず型をSystem.EventHandlerにしてしまう
        let begin = self.idx - 1;
        let ident = format!("<{}>lambda_{}", self.current_fn().name, crate::seq!());
        let ty = Type::Class(ClassKind::Class, Some("System.Runtime".to_string()), vec!["System".to_string()], "EventHandler".to_string(), None, false);
        if self.current_fn().nested_class.is_none() {
            // nestedクラスを初期化
            // コンストラクタを自動生成
            // let mut ctor = Function::new(".ctor", true);
            let ty = RRType::new(Type::Class(ClassKind::NestedClass(self.current_class.last().unwrap().to_string()), None, self.current_mod.to_vec(), "<>c__DisplayClass0_0".to_string(), None, true));
            // let mut selfobj = Object::new("".to_string(), 0, ObjectKind::Param, RRType::clone(&ty), true);
            // selfobj.assigned = true;
            // let instance = new_variable_node(&Rc::new(RefCell::new(selfobj)), &[]);
            // ctor.statements = new_method_call_node(instance, ".ctor".to_string(), vec![], &[]);
            //self.current_fn_mut().local_funcs.push(ctor.clone());
            //let mut im = Impl::new("<>c__DisplayClass0_0".to_string(), self.current_mod.to_vec(), None);
            //im.functions.push(Rc::new(ctor));
            // クラスの生成
            let displayclass = Class::new(ClassKind::NestedClass(self.current_class.last().unwrap().to_string()), "<>c__DisplayClass0_0".to_string(), self.current_mod.to_vec(), None);
            //let self_ty = RRType::new(Type::_Self(self.current_mod.to_vec(), "<>c__DisplayClass0_0".to_string(), true));
            //let self_obj = Rc::new(RefCell::new(Object::new("self".to_string(), 0, ObjectKind::Param, self_ty, true)));
            //self_obj.borrow_mut().assigned = true;
            //displayclass.field.push(self_obj);
            //displayclass.impls.push(Rc::new(im));
            self.current_fn_mut().nested_class = Some(Rc::new(RefCell::new(displayclass)));
            let instance_name = format!("<{}>nested_class", self.current_fn().name.to_string());
            let nested_class_instance = new_variable_node_with_let(
                &mut self.current_fn_mut().symbol_table.borrow_mut(),
                //format!("<{}>nested_class", current_fn_name),
                instance_name,
                RRType::clone(&ty),
                &[],
                true,
                true,
                ObjectKind::Local,
            );  // <- こいつの.ctorを呼ぶ必要がある
            self.nested_class_instance = Some(nested_class_instance);
        }
        self.current_lambda = Some(Function::new(&ident, false));
        let stmts = self.parse_expr();
        let mut lambda = self.current_lambda.take().unwrap();
        lambda.statements = stmts;
        self.current_fn_mut().local_funcs.push(lambda);
        new_lambda_node(ty, ident, &self.tokens[begin..self.idx])
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
            unreachable!();
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
                node = if self.eat(TokenKind::Gt) {
                    if self.eat(TokenKind::Assign) {
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
                        new_binary_op_node(
                            BinaryOpKind::Shr,
                            node,
                            self.parse_add(),
                            &self.tokens[begin..self.idx],
                        )
                    }
                } else if self.eat(TokenKind::Assign) {
                    new_binary_op_node(
                        BinaryOpKind::Ge,
                        node,
                        self.parse_bitor(),
                        &self.tokens[begin..self.idx],
                    )
                } else {
                    new_binary_op_node(
                        BinaryOpKind::Gt,
                        node,
                        self.parse_bitor(),
                        &self.tokens[begin..self.idx],
                    )
                };
            } else if self.eat(TokenKind::Ge) {
                unreachable!();
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
                unreachable!();
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
                if let Some(ty) = self.type_no_bounds() {
                    node = new_cast_node(ty, node, &self.tokens[begin..self.idx]);
                } else {
                    self.bump();
                }
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
                if self.eat(TokenKind::OpenDelim(Delimiter::Parenthesis)) {
                    // method
                    let start_paren = self.idx - 1;
                    let mut args = vec![];
                    while !self.eat(TokenKind::CloseDelim(Delimiter::Parenthesis)) && !self.is_eof() {
                        args.push(self.parse_expr());
                        if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
                            e0010(Rc::clone(&self.errors), self.errorset());
                            self.close_delimiter(Delimiter::Parenthesis, self.tokens[start_paren].clone());
                            break;
                        }
                    }
                    node = new_method_call_node(
                        node,
                        ident,
                        args,
                        &self.tokens[begin..self.idx],
                    );
                } else {
                    node = new_field_or_property_node(
                        Rc::clone(&self.current_fn().symbol_table),
                        node,
                        ident,
                        &self.tokens[begin..self.idx],
                    );
                }
            } else {
                return node;
            }
        }
    }

    fn parse_term(&mut self) -> Node<'a> {
        if self.eat(TokenKind::OpenDelim(Delimiter::Parenthesis)) {
            let except_struct_expression = self.except_struct_expression;
            self.except_struct_expression = false;
            let node = self.parse_expr();
            self.expect(TokenKind::CloseDelim(Delimiter::Parenthesis));
            self.except_struct_expression = except_struct_expression;
            return node;
        } else if self.eat(TokenKind::OpenDelim(Delimiter::Brace)) {
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
            TokenKind::Keyword(Keyword::Box) => {
                self.idx += 1;
                self.parse_box()
            }
            TokenKind::Builtin(kind) => {
                self.idx += 1;
                self.parse_builtin(kind)
            }
            _ => {
                e0006(Rc::clone(&self.errors), self.errorset());
                self.idx += 1;
                new_empty_node()
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
        } else if self.eat(TokenKind::OpenDelim(Delimiter::Parenthesis)) {
            // function
            let begin = self.idx-2;
            let mut args = vec![];
            while !self.eat(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
                args.push(self.parse_expr());
                if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
                    e0010(Rc::clone(&self.errors), self.errorset());
                    self.close_delimiter(Delimiter::Parenthesis, self.tokens[begin+1].clone());
                    break;
                }
            }
            new_function_call_node(name, args, &self.tokens[begin..self.idx])
        } else if !self.except_struct_expression && self.eat(TokenKind::OpenDelim(Delimiter::Brace)) {
            // struct expression
            let begin = self.idx-2;
            let mut field = vec![];
            while !self.eat(TokenKind::CloseDelim(Delimiter::Brace)) {
                let start_brace = self.idx-1;
                field.push(self.parse_expr());
                if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Brace)) {
                    e0009(Rc::clone(&self.errors), self.errorset());
                    self.close_delimiter(Delimiter::Brace, self.tokens[start_brace].clone());
                    break;
                }
            }
            let tokens = &self.tokens[begin..self.idx];
            let current_mod = self.current_mod.to_vec();
            let reference = self.foreign_reference.clone();
            new_struct_expr_node(
                &mut self.current_fn_mut().symbol_table.borrow_mut(),
                reference,
                name,
                field,
                tokens,
                current_mod,
            )
        } else {
            // local variable or parameter
            if let Some(local_fn) = &self.current_lambda {
                if let Some(obj) = local_fn.symbol_table.borrow().find(name) {
                    new_variable_node(obj, &self.tokens[self.idx-1..self.idx])
                } else {
                    //let symbol_table = Rc::clone(&self.current_fn().symbol_table);
                    let current_fn = self.current_fn.as_mut().unwrap();
                    //let old_obj = current_fn.symbol_table.borrow_mut().drain(name);
                    //let node = if let Some(old_obj) = current_fn.symbol_table.borrow_mut().drain(name) {
                    //let node = if let Some(old_obj) = old_obj {
                    let node = if let Some(old_obj) = current_fn.symbol_table.borrow().find(name) {  // 理想はdrainでローカル変数を削除だが、オフセットがずれてしまうのでとりあえずfindで <- というかローカル変数生成時にObjectType::Localで絞っているからわざわざdrainしなくても良いのでは？というかdrainしなくてもオフセットがずれてしまうのでは？
                        // WIP
                        // 親メソッド内のローカル変数をnestedクラスのフィールド変数に置き換え
                        // 親メソッド: count(local) => nested_class.count(field)
                        //                             -> ldloc nested_class
                        //                             -> ldfld count
                        // 子メソッドは自身のインスタンスをロードして、そのフィールド変数を参照
                        // 子メソッド:              => this.count(field)
                        //                             -> ldarg.0
                        //                             -> ldfld count
                        // old_objはシンボルテーブルからは削除するが、NodeにRc::cloneされたものがある

                        // 変数の定義場所をnestedクラスのフィールド変数に置き換え
                        let (ident, ty, is_assigned, is_mutable) = {
                            let obj = old_obj.borrow();
                            (obj.name.to_string(), RRType::clone(&obj.ty), obj.assigned, obj.mutable)
                        };
                        let field_offset = current_fn.nested_class.as_ref().unwrap().borrow().field.offset(ObjectKind::Field);
                        let new_obj = Rc::new(RefCell::new(Object::new(ident.to_string(), field_offset, ObjectKind::Field, RRType::clone(&ty), is_mutable)));
                        new_obj.borrow_mut().assigned = is_assigned;
                        current_fn.nested_class.as_mut().unwrap().borrow_mut().field.push(Rc::clone(&new_obj));

                        // old_objをnestedクラスのフィールド変数を指すように変更
                        {
                            *old_obj.borrow_mut() = new_obj.borrow().clone();
                            let instance_name = format!("<{}>nested_class", current_fn.name);
                            let symbol_table = current_fn.symbol_table.borrow();
                            let parent_obj = symbol_table.find(&instance_name).unwrap();
                            old_obj.borrow_mut().kind = ObjectKind::Local;  // オフセットのずれ修正（TODO:削除）
                            old_obj.borrow_mut().parent = Some(Rc::clone(parent_obj));
                        }

                        //let ty = RRType::new(Type::_Self(self.current_mod.to_vec(), "<>c__DisplayClass0_0".to_string(), true));
                        let ty = RRType::new(Type::Class(ClassKind::NestedClass(self.current_class.last().unwrap().to_string()), None, self.current_mod.to_vec(), "<>c__DisplayClass0_0".to_string(), None, true));
                        let self_obj = Rc::new(RefCell::new(Object::new("self".to_string(), 0, ObjectKind::Param, ty, true)));
                        self_obj.borrow_mut().assigned = true;
                        let self_node = new_variable_node(&self_obj, &[]);
                        new_field_or_property_node(
                            Rc::clone(&current_fn.symbol_table),
                            self_node,
                            ident,
                            &self.tokens[self.idx-1..self.idx],
                        )
                    } else if let Some(obj) = current_fn.nested_class.as_ref().unwrap().borrow().field.find(name) {
                        new_variable_node(obj, &self.tokens[self.idx-1..self.idx])
                    } else {
                        e0007(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), name);
                        new_empty_node()
                    };
                    node
                }
            } else {
                if let Some(obj) = self.current_fn().symbol_table.borrow().find(name) {
                    new_variable_node(obj, &self.tokens[self.idx-1..self.idx])
                } else {
                    e0007(Rc::clone(&self.errors), (self.path, &self.lines, &self.tokens[self.idx-1..self.idx]), name);
                    new_empty_node()
                }
            }
        }
    }

    fn parse_box(&mut self) -> Node<'a> {
        let begin = self.idx-1;
        self.expect(TokenKind::PathSep);
        let ident = self.expect_ident();
        new_box_node(
            self.parse_ident(&ident),
            &self.tokens[begin..self.idx],
        )
    }

    fn parse_builtin(&mut self, kind: &Builtin) -> Node<'a> {
        let begin = self.idx-1;
        self.expect(TokenKind::Not);
        self.expect(TokenKind::OpenDelim(Delimiter::Parenthesis));
        let mut args = vec![];
        while !self.eat(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
            args.push(self.parse_expr());
            if !self.eat(TokenKind::Comma) && !self.check(TokenKind::CloseDelim(Delimiter::Parenthesis)) {
                e0010(Rc::clone(&self.errors), self.errorset());
            }
        }
        new_builtin_call_node(*kind, args, &self.tokens[begin..self.idx])
    }

    fn parse_simple_path(&mut self, segment: &str) -> Node<'a> {
        let begin = self.idx-2;
        if self.eat_keyword(Keyword::Ctor) {
            new_path_node(
                segment,
                self.parse_ident(".ctor"),
                &self.tokens[begin..self.idx],
            )
        } else {
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
}
