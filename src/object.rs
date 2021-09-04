#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    pub name: String,
    pub offset: usize,
}

impl Object {
    pub fn new(name: String, offset: usize) -> Self {
        Object { name, offset }
    }
}

#[derive(Debug)]
pub struct SymbolTable<'a> {
    objs: Vec<&'a Object>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        SymbolTable { objs: vec![] }
    }

    pub fn push(&mut self, obj: &'a Object) {
        self.objs.push(obj);
    }
}
