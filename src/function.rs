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
    pub is_static: bool,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Function {
            name: name.to_string(),
            rettype: Type::Void,
            statements: new_block_node(vec![]),
            lvar_symbol_table: SymbolTable::new(),
            param_symbol_table: SymbolTable::new(),
            is_static: true,
        }
    }
}

impl FindSymbol for [Function] {
    type Item = Function;

    fn find(&self, name: &str) -> Option<&Self::Item> {
        self.iter().find(|f|f.name == name)
    }

    fn find_mut(&mut self, name: &str) -> Option<&mut Self::Item> {
        self.iter_mut().find(|f|f.name == name)
    }
}
