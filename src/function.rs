use super::ast::*;
use super::object::*;
use super::keyword::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub rettype: Type,
    pub statements: Node,
    pub lvar_symbol_table: SymbolTable,
    pub param_symbol_table: SymbolTable,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Function {
            name: name.to_string(),
            rettype: Type::Void,
            statements: Node::Block { stmts: vec![] },
            lvar_symbol_table: SymbolTable::new(),
            param_symbol_table: SymbolTable::new(),
        }
    }
}

pub trait FunctionSymbolTable {
    fn find_function(&self, name: &str) -> Option<&Function>;
}

impl FunctionSymbolTable for [Function] {
    fn find_function(&self, name: &str) -> Option<&Function> {
        self.iter().find(|f|f.name == name)
    }
}

