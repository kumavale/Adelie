use crate::ast::Attribute;
use crate::class::ClassKind;
use crate::error::Errors;
use crate::keyword::{Type, RRType};
use crate::namespace::NameSpace;
use crate::object::Object;
use std::borrow::Cow;
use std::cell::RefCell;
use std::fs::File;
use std::io::{Write, BufWriter, Result};
use std::path::Path;
use std::rc::{Rc, Weak};

#[derive(Clone, Debug)]
pub struct IlClass {
    name: String,
    kind: ClassKind,
    fields: Vec<String>,
    funcs: Vec<IlFunc>,
    nested: Vec<IlClass>,
}
impl IlClass {
    pub fn new(name: &str, kind: ClassKind, fields: Vec<String>, funcs: Vec<IlFunc>, nested: Vec<IlClass>) -> Self {
        IlClass {
            name: name.to_string(),
            kind,
            fields,
            funcs,
            nested,
        }
    }
    fn write_il<W: Write>(&self, writer: &mut BufWriter<W>) -> Result<()> {
        match self.kind {
            ClassKind::Struct => {
                writeln!(writer, ".class private sequential auto sealed beforefieldinit '{}' extends [mscorlib]System.ValueType {{", self.name)?;
            }
            ClassKind::Class => {
                writeln!(writer, ".class private auto ansi abstract sealed beforefieldinit '{}' extends [mscorlib]System.Object {{", self.name)?;
            }
            ClassKind::NestedClass(_) => {
                writeln!(writer, ".class nested private auto ansi sealed beforefieldinit '{}' extends [mscorlib]System.Object {{", self.name)?;
            }
        }

        for nc in &self.nested {
            nc.write_il(writer)?;
        }

        // TODO: classは全て`.ctor`を作成する。
        if self.name == "<>c__DisplayClass0_0" {
            writeln!(writer, "\t.method public hidebysig specialname rtspecialname instance void .ctor() cil managed {{")?;
            writeln!(writer, "\t\tldarg.0")?;
            writeln!(writer, "\t\tcall instance void [mscorlib]System.Object::.ctor()")?;
            writeln!(writer, "\t\tret")?;
            writeln!(writer, "\t}}")?;
        }

        for field in &self.fields {
            writeln!(writer, "\t{}", field)?;
        }

        for func in &self.funcs {
            func.write_il(writer)?;
        }

        writeln!(writer, "}}")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct IlFunc {
    name: String,
    is_static: bool,
    rettype: RRType,
    params: String,
    locals: String,
    inits: Vec<Rc<RefCell<Object>>>,
    stmts: Vec<String>,
}
impl IlFunc {
    pub fn new(
        name: &str,
        is_static: bool,
        rettype: RRType,
        params: String,
        locals: String,
        inits: Vec<Rc<RefCell<Object>>>,
        stmts: Vec<String>,
    ) -> Self {
        IlFunc {
            name: name.to_string(),
            is_static,
            rettype,
            params,
            locals,
            inits,
            stmts,
        }
    }
    fn write_il<W: Write>(&self, writer: &mut BufWriter<W>) -> Result<()> {
        if self.name == "main" {
            writeln!(writer, ".method static void Main() cil managed {{")?;
            writeln!(writer, "\t.entrypoint")?;
        } else {
            writeln!(writer, ".method assembly {} {} '{}'({}) cil managed {{",
                if self.is_static { "static" } else { "instance" },
                self.rettype.borrow().to_ilstr(),
                self.name,
                self.params)?;
        }
        writeln!(writer, "\t.maxstack 32")?;
        if !self.locals.is_empty() {
            writeln!(writer, "\t.locals init (")?;
            writeln!(writer, "{}", self.locals)?;
            writeln!(writer, "\t)")?;
        }
        for obj in self.inits.iter() {
            if let Type::Class(ClassKind::NestedClass(pn), .., name, _, _) = &*obj.borrow().ty.borrow() {
                if name == "<>c__DisplayClass0_0" {
                    writeln!(writer, "\tnewobj instance void '{}'/'<>c__DisplayClass0_0'::.ctor()", pn)?;
                    writeln!(writer, "\tstloc '{}'", obj.borrow().name)?;
                }
            }
        }
        for stmt in &self.stmts {
            writeln!(writer, "{}", stmt)?;
        }
        writeln!(writer, "\tret")?;
        writeln!(writer, "}}")?;
        Ok(())
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
    fn write_il<W: Write>(&self, writer: &mut BufWriter<W>) -> Result<()> {
        writeln!(writer, ".assembly '{}' {{}}", self.name)?;
        for asm in &self.asms {
            writeln!(writer, ".assembly extern '{}' {{", asm.name)?;
            if let Some(pkt) = &asm.pkt {
                writeln!(writer, "    .publickeytoken = ({})", pkt)?;
            }
            writeln!(writer, "}}")?;
        }
        Ok(())
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
    fn write_il<W: Write>(&self, writer: &mut BufWriter<W>) -> Result<()> {
        writeln!(writer, ".class private auto ansi sealed '{}' extends [mscorlib]System.Enum {{", self.name)?;
        for field in &self.fields {
            writeln!(writer, "\t{}", field)?;
        }
        writeln!(writer, "}}")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct Il {
    mani: Option<IlManifest>,
    stmts: Vec<String>,
    enums: Vec<IlEnum>,
    funcs: Vec<IlFunc>,
    classes: Vec<IlClass>,
    nested_classes: Vec<IlClass>,
}
impl Il {
    pub fn new() -> Self {
        Il {
            mani: None,
            stmts: vec![],
            enums: vec![],
            funcs: vec![],
            classes: vec![],
            nested_classes: vec![],
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

    pub fn push_il_class(&self, ilclass: IlClass) {
        if let ClassKind::NestedClass(_) = ilclass.kind {
            self.il.borrow_mut().nested_classes.push(ilclass);
        } else {
            self.il.borrow_mut().classes.push(ilclass);
        }
    }

    pub fn drain_il_stmts(&self) -> Vec<String> {
        self.il.borrow_mut().stmts.drain(..).collect()
    }

    pub fn drain_il_funcs(&self) -> Vec<IlFunc> {
        self.il.borrow_mut().funcs.drain(..).collect()
    }

    pub fn drain_il_nested_classes(&self) -> Vec<IlClass> {
        self.il.borrow_mut().nested_classes.drain(..).collect()
    }

    pub fn write_il(&self) -> Result<()> {
        let path = Path::new(&self.path).with_extension("il");
        let mut writer = BufWriter::new(File::create(path)?);

        if let Some(mani) = &self.il.borrow().mani {
            mani.write_il(&mut writer)?;
        }
        for enu in &self.il.borrow().enums {
            enu.write_il(&mut writer)?;
        }
        for class in &self.il.borrow().classes {
            class.write_il(&mut writer)?;
        }
        writer.flush()?;
        Ok(())
    }
}
