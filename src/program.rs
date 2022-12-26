use crate::ast::Attribute;
use crate::class::ClassKind;
use crate::error::Errors;
use crate::keyword::{Type, RRType};
use crate::namespace::NameSpace;
use crate::object::Object;
use std::borrow::Cow;
use std::cell::RefCell;
use std::path::Path;
use std::rc::{Rc, Weak};

#[derive(Clone, Debug)]
pub struct IlMethod {
    name: String,
    is_static: bool,
    rettype: RRType,
    params: String,
    locals: String,
    inits: Vec<Rc<RefCell<Object>>>,
}
impl IlMethod {
    pub fn new(name: &str, is_static: bool, rettype: RRType, params: String, locals: String, inits: Vec<Rc<RefCell<Object>>>) -> Self {
        IlMethod {
            name: name.to_string(),
            is_static,
            rettype,
            params,
            locals,
            inits,
        }
    }
    fn display_il(&self) {
        println!("\t.method assembly {} {} '{}'({}) cil managed {{",
            if self.is_static {"static"} else {"instance"},
            self.rettype.borrow().to_ilstr(),
            self.name,
            self.params);
        println!("\t.maxstack 32");
        if !self.locals.is_empty() {
            println!("\t.locals init (");
            println!("{}", self.locals);
            println!("\t)");
        }
        self.inits.iter().for_each(|obj| if let Type::Class(ClassKind::NestedClass(pn), .., name, _, _) = &*obj.borrow().ty.borrow() {
            if name == "<>c__DisplayClass0_0" {
                println!("\tnewobj instance void '{}'/'<>c__DisplayClass0_0'::.ctor()", pn);
                println!("\tstloc '{}'", obj.borrow().name);
            }
        });
    }
}

#[derive(Clone, Debug)]
pub struct IlFunc {
    name: String,
    rettype: RRType,
    params: String,
    locals: String,
    inits: Vec<Rc<RefCell<Object>>>,
}
impl IlFunc {
    pub fn new(name: &str, rettype: RRType, params: String, locals: String, inits: Vec<Rc<RefCell<Object>>>) -> Self {
        IlFunc {
            name: name.to_string(),
            rettype,
            params,
            locals,
            inits,
        }
    }
    fn display_il(&self) {
        if self.name == "main" {
            println!(".method static void Main() cil managed {{");
            println!("\t.entrypoint");
        } else {
            println!(".method static {} '{}'({}) cil managed {{", self.rettype.borrow().to_ilstr(), self.name, self.params);
        }
        println!("\t.maxstack 32");
        if !self.locals.is_empty() {
            println!("\t.locals init (");
            println!("{}", self.locals);
            println!("\t)");
        }
        self.inits.iter().for_each(|obj| if let Type::Class(ClassKind::NestedClass(pn), .., name, _, _) = &*obj.borrow().ty.borrow() {
            if name == "<>c__DisplayClass0_0" {
                println!("\tnewobj instance void '{}'/'<>c__DisplayClass0_0'::.ctor()", pn);
                println!("\tstloc '{}'", obj.borrow().name);
            }
        });
    }
}

#[derive(Clone, Debug)]
struct IlAsm {
    name: String,
    pkt: Option<String>,
}
impl IlAsm {
    pub fn new(name: &str, pkt: Option<&str>) -> Self {
        IlAsm {
            name: name.to_string(),
            pkt: pkt.map(|s|s.to_string()),
        }
    }
}
#[derive(Clone, Debug)]
pub struct IlManifest {
    name: String,
    asms: Vec<IlAsm>,
}
impl IlManifest {
    pub fn new(name: &str) -> Self {
        IlManifest {
            name: name.to_string(),
            asms: vec![],
        }
    }
    pub fn push_asm(&mut self, name: &str, pkt: Option<&str>) {
        let ilasm = IlAsm::new(name, pkt);
        self.asms.push(ilasm);
    }
    pub fn display_il(&self) {
        println!(".assembly '{}' {{}}",  self.name);
        for asm in &self.asms {
            println!(".assembly extern '{}' {{", asm.name);
            if let Some(pkt) = &asm.pkt {
                println!("    .publickeytoken = ({})", pkt);
            }
            println!("}}");
        }
    }
}

#[derive(Clone, Debug)]
pub struct IlEnum {
    name: String,
    fields: Vec<String>,  // TODO: Vec<IlField>
}
impl IlEnum {
    pub fn new(name: &str) -> Self {
        IlEnum {
            name: name.to_string(),
            fields: vec![],
        }
    }
    pub fn push_field<'a, S: Into<Cow<'a, str>>>(&mut self, s: S) {
        self.fields.push(s.into().into_owned());
    }
    pub fn display_il(&self) {
        println!(".class private auto ansi sealed '{}' extends [mscorlib]System.Enum {{", self.name);
        for field in &self.fields {
            println!("\t{}", field);
        }
        println!("}}");
    }
}

#[derive(Clone, Debug)]
pub struct Il {
    mani: Option<IlManifest>,
    stmts: Vec<String>,
    enums: Vec<IlEnum>,
    funcs: Vec<IlFunc>,
    methods: Vec<IlMethod>,
}
impl Il {
    pub fn new() -> Self {
        Il {
            mani: None,
            stmts: vec![],
            enums: vec![],
            funcs: vec![],
            methods: vec![],
        }
    }
}

#[derive(Clone)]
pub struct Program<'a> {
    // TODO: Files { path, lines }
    pub name: String,
    pub path: &'a str,
    pub lines: Vec<&'a str>,
    pub namespace: Rc<RefCell<NameSpace<'a>>>,
    pub current_namespace: Rc<RefCell<NameSpace<'a>>>,
    pub errors: Rc<RefCell<Errors>>,
    pub il: RefCell<Il>,
    pub references: Vec<Attribute>,
    pub ret_address: RefCell<bool>,
}

impl<'a> Program<'a> {
    pub fn new(path: &'a str, input: &'a str, errors: Rc<RefCell<Errors>>) -> Self {
        let namespace = Rc::new(RefCell::new(NameSpace::new("crate", None)));
        Program {
            name: Path::new(&path)
                .file_stem()
                .and_then(|n|n.to_str())
                .map(|n|n.to_string())
                .unwrap_or_default(),
            path,
            lines: input.lines().collect(),
            namespace: Rc::clone(&namespace),
            current_namespace: namespace,
            errors,
            il: RefCell::new(Il::new()),
            references: vec![],
            ret_address: RefCell::new(false),
        }
    }

    pub fn enter_namespace(&mut self, id: &str) {
        let same_ns = self.current_namespace.borrow().children
            .iter()
            .find(|child| child.borrow().name == id)
            .map(Rc::clone);
        if let Some(ns) = same_ns {
            self.current_namespace = ns;
        } else {
            let child = Rc::new(RefCell::new(NameSpace::new(id, Some(Rc::clone(&self.current_namespace)))));
            self.current_namespace.borrow_mut().children.push(Rc::clone(&child));
            self.current_namespace = child;
        }
    }

    pub fn leave_namespace(&mut self) {
        let parent = Weak::clone(&self.current_namespace.borrow().parent);
        self.current_namespace = Rc::clone(&parent.upgrade().unwrap());
    }

    pub fn push_il_text<S: Into<Cow<'a, str>>>(&self, s: S) {
        let text = s.into();
        self.il.borrow_mut().stmts.push(text.into_owned());
    }

    pub fn push_il_enum(&self, ilenum: IlEnum) {
        self.il.borrow_mut().enums.push(ilenum);
    }

    pub fn push_il_mani(&self, ilman: IlManifest) {
        self.il.borrow_mut().mani = Some(ilman);
    }

    pub fn push_il_func(&self, ilfunc: IlFunc) {
        self.il.borrow_mut().funcs.push(ilfunc);
    }

    pub fn push_il_method(&self, ilmethod: IlMethod) {
        self.il.borrow_mut().methods.push(ilmethod);
    }

    pub fn clear_il(&self) {
        self.il.borrow_mut().mani.take();
        self.il.borrow_mut().stmts.clear();
        self.il.borrow_mut().enums.clear();
        self.il.borrow_mut().funcs.clear();
        self.il.borrow_mut().methods.clear();
    }

    pub fn display_il(&self) {
        if let Some(mani) = &self.il.borrow().mani {
            mani.display_il();
        }
        for enu in &self.il.borrow().enums {
            enu.display_il();
        }

        // 仮実装
        if self.il.borrow().funcs.is_empty() {
            for method in &self.il.borrow().methods {
                method.display_il();
                for stmt in &self.il.borrow().stmts {
                    println!("{}", stmt);
                }
                println!("\tret");
                println!("}}");
            }
        } else {
            for func in &self.il.borrow().funcs {
                func.display_il();
                for stmt in &self.il.borrow().stmts {
                    println!("{}", stmt);
                }
                println!("\tret");
                println!("}}");
            }
        }
        self.clear_il();
    }
}
