use super::ast::*;
use super::object::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub statements: Option<Node>,
    pub lvar_symbol_table: SymbolTable,
    pub param_symbol_table: SymbolTable,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Function {
            name: name.to_string(),
            statements: None,
            lvar_symbol_table: SymbolTable::new(),
            param_symbol_table: SymbolTable::new(),
        }
    }
}
