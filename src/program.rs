use super::class::*;
use super::function::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            functions: vec![],
            structs: vec![],
        }
    }

    pub fn find_function(&self, name: &str) -> Option<&Function> {
        self.functions.find_function(name)
    }

    pub fn find_struct(&self, name: &str) -> Option<&Struct> {
        self.structs.find_struct(name)
    }
}
