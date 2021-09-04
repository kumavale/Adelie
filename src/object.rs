#[derive(Debug)]
pub struct Object {
    name: String,
    offset: usize,
}

impl Object {
    pub fn new(name: String, offset: usize) -> Self {
        Object { name, offset }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    objs: Vec<Object>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { objs: vec![] }
    }
}
