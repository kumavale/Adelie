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

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionSymbolTable {
    pub functions: Vec<(String, SymbolTable)>,
}

impl FunctionSymbolTable {
    pub fn new(code_ast: &[Function]) -> Self {
        FunctionSymbolTable {
            functions: code_ast.iter().map(|f|(f.name.clone(), f.param_symbol_table.clone())).collect(),
        }
    }

    pub fn params(&self, name: &str) -> Option<&SymbolTable> {
        for f in &self.functions {
            if f.0 == name {
                return Some(&f.1);
            }
        }
        None
    }
}

