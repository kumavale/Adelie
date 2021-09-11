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
}
