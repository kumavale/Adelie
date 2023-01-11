use crate::ast::*;
use crate::builtin::*;
use crate::class::ClassKind;
use crate::error::*;
use crate::function::Function;
use crate::keyword::{Type, RRType, Numeric, Float, FloatNum, Keyword};
use crate::namespace::NameSpace;
use crate::object::{FindSymbol, Object, ObjectKind, EnumObject, SymbolTable};
use crate::program::Program;
use crate::token::Token;
use std::rc::Rc;
use std::cell::Ref;

type Result<T> = std::result::Result<T, ()>;

pub fn gen_il<'a>(node: Node, st: &SymbolTable, p: &'a Program<'a>) -> Result<RRType> {
    match node.kind {
        NodeKind::Integer { ty, num } => {
            gen_il_integer(node.token, st, p, ty, num)
        }
        NodeKind::Float { ty, num } => {
            gen_il_float(node.token, st, p, ty, num)
        }
        NodeKind::String { ty, str } => {
            gen_il_string(node.token, st, p, ty, &str)
        }
        NodeKind::Box { method } => {
            gen_il_box(node.token, st, p, *method)
        }
        NodeKind::Builtin { kind, args } => {
            gen_il_builtin(node.token, st, kind, args, p)
        }
        NodeKind::Let { obj, init } => {
            gen_il_let(node.token, st, p, obj.borrow(), init)
        }
        NodeKind::Call { name, args } => {
            gen_il_call(node.token, st, p, &name, args)
        }
        NodeKind::Method { expr, ident, args } => {
            gen_il_method(node.token, st, p, *expr, &ident, args)
        }
        NodeKind::Lambda { ty, ident, parent } => {
            gen_il_lambda(node.token, st, p, ty, &ident, &parent)
        }
        NodeKind::Struct { obj, field } => {
            gen_il_struct(node.token, st, p, obj.borrow(), field)
        }
        NodeKind::Field { expr, ident } => {
            gen_il_field(node.token, st, p, *expr, &ident)
        }
        NodeKind::Variable { obj } => {
            gen_il_variable(node.token, st, p, obj.borrow())
        }
        NodeKind::Enum { obj } => {
            gen_il_enum(node.token, st, p, obj)
        }
        NodeKind::Block { stmts } => {
            gen_il_block(node.token, st, p, stmts)
        }
        NodeKind::If { cond, then, els } => {
            gen_il_if(node.token, st, p, *cond, *then, els)
        }
        NodeKind::While { cond, then, brk_label_seq } => {
            gen_il_while(node.token, st, p, *cond, *then, brk_label_seq)
        }
        NodeKind::Loop { then, brk_label_seq } => {
            gen_il_loop(node.token, st, p, *then, brk_label_seq)
        }
        NodeKind::Assign { lhs, rhs } => {
            gen_il_assign(node.token, st, p, *lhs, *rhs)
        }
        NodeKind::Return { expr, func_retty } => {
            gen_il_return(node.token, st, p, expr, func_retty)
        }
        NodeKind::Break { brk_label_seq } => {
            gen_il_break(node.token, st, p, brk_label_seq)
        }
        NodeKind::Cast { ty: new_type, expr } => {
            gen_il_cast(node.token, st, p, new_type, *expr)
        }
        NodeKind::UnaryOp { kind, expr } => {
            gen_il_unaryop(node.token, st, p, kind, *expr)
        }
        NodeKind::BinaryOp { kind, lhs, rhs } => {
            gen_il_binaryop(node.token, st, p, kind, *lhs, *rhs)
        }
        NodeKind::ShortCircuitOp { kind, lhs, rhs } => {
            gen_il_shortcircuitop(node.token, st, p, kind, *lhs, *rhs)
        }
        NodeKind::Semi { expr } => {
            gen_il_semi(node.token, st, p, *expr)
        }
        NodeKind::Path { segment, child } => {
            gen_il_path(node.token, st, p, &segment, vec![segment.to_string()], *child)
        }
        NodeKind::Empty => {
            gen_il_empty()
        }
    }
}

fn label_seq() -> usize {
    unsafe {
        static mut ID: usize = 0;
        ID += 1;
        ID
    }
}

fn gen_il_integer(current_token: &[Token], _st: &SymbolTable, p: &Program, ty: RRType, num: i128) -> Result<RRType> {
    use super::token::*;
    debug_assert!(matches!(current_token[0].kind,
            TokenKind::Literal(LiteralKind::Char(_))
            | TokenKind::Literal(LiteralKind::Integer(_))
            | TokenKind::Keyword(Keyword::True)
            | TokenKind::Keyword(Keyword::False)));
    p.push_il_text(format!("\tldc.i4 {}", num as i32));
    Ok(ty)
}

fn gen_il_float(_current_token: &[Token], _st: &SymbolTable, p: &Program, ty: RRType, num: FloatNum) -> Result<RRType> {
    match num {
        FloatNum::Float32(f) => p.push_il_text(format!("\tldc.r4 {}", f)),
    }
    Ok(ty)
}

fn gen_il_string(_current_token: &[Token], _st: &SymbolTable, p: &Program, ty: RRType, str: &str) -> Result<RRType> {
    p.push_il_text(format!("\tldstr \"{}\"", str));
    Ok(ty)
}

fn gen_il_box<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, method: Node) -> Result<RRType> {
    if let NodeKind::Call { name, args } = method.kind {
        match name.as_str() {
            "new" => {
                let boxed_ty = gen_il(args.into_iter().next().unwrap(), st, p)?;
                p.push_il_text(format!("\tbox {}", boxed_ty.borrow().to_ilstr()));
                Ok(RRType::new(Type::Box(boxed_ty)))
            }
            _ => unreachable!()
        }
    } else {
        unreachable!()
    }
}

fn gen_il_let<'a>(current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, obj: Ref<Object>, init: Option<Box<Node>>) -> Result<RRType> {
    fn check_type(lty: &Type, rty: &Type) -> Result<()> {
        match (lty, rty) {
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if lty == rty => Ok(()),
            _ => Err(())
        }
    }
    if let Some(init) = init {
        if let Some(parent) = &obj.parent {
            let ident = obj.name.to_string();
            *p.ret_address.borrow_mut() = true;
            *p.consume.borrow_mut() = false;
            let parent_ty = gen_il_variable(current_token, st, p, parent.borrow())?;
            let parent_ty = parent_ty.borrow();
            *p.consume.borrow_mut() = true;
            *p.ret_address.borrow_mut() = false;
            match &*parent_ty {
                Type::Class(_, _, ref path, ref name, _, _) => {
                    let namespace = p.namespace.borrow();
                    let ns = namespace.find(path).unwrap();
                    if let Some(cl) = ns.find_class(|_|true, name) {
                        if let Some(field) = cl.borrow().field.find(&ident) {
                            let rty = gen_il(*init, st, p)?;
                            if check_type(&obj.ty.borrow(), &rty.borrow()).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.ty.borrow(), &rty.borrow());
                            }
                            p.push_il_text(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        }
                    }
                }
                _ => unreachable!(),
            }
        } else {
            let rty = gen_il(*init, st, p)?;
            if check_type(&obj.ty.borrow(), &rty.borrow()).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.ty.borrow(), &rty.borrow());
            }
            p.push_il_text(format!("\tstloc {}", obj.offset));
        }
    } else {
        // Do nothing
    }
    Ok(RRType::new(Type::Void))
}

fn gen_il_call<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, name: &str, args: Vec<Node>) -> Result<RRType> {
    fn check_type(arg: &Type, param: &Type) -> Result<()> {
        match (arg, param) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if arg == param => Ok(()),
            _ => Err(())
        }
    }
    let func = p.namespace.borrow().find_fn(name).unwrap();
    let objs = &func
        .symbol_table
        .borrow()
        .objs;
    let params = objs
        .iter()
        .filter(|o| o.borrow().kind == ObjectKind::Param)
        .collect::<Vec<_>>();
    for (arg, param) in args.into_iter().zip(&params) {
        let token = arg.token;
        let param = param.borrow();
        let param_ty = &param.ty.borrow();
        let arg_ty = gen_il(arg, st, p)?;
        if check_type(&arg_ty.borrow(), param_ty).is_err() {
            e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty.borrow());
        }
    }
    let params = params
        .iter()
        .map(|p|p.borrow().ty.borrow().to_ilstr())
        .collect::<Vec<String>>()
        .join(", ");
    p.push_il_text(format!("\tcall {} '{}'::'{}'({})", func.rettype.borrow().to_ilstr(), p.name, name, params));
    Ok(func.rettype.clone())
}

fn gen_il_method<'a>(
    current_token: &[Token],
    st: &SymbolTable,
    p: &'a Program<'a>,
    expr: Node,
    ident: &str,
    args: Vec<Node>,
) -> Result<RRType> {
    fn check_type(arg_ty: &Type, param_ty: &Type) -> Result<()> {
        match (arg_ty, param_ty) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if arg_ty == param_ty => Ok(()),
            (Type::Class(.., base_ty, _), Type::Class(..)) => {
                fn check_base(base_ty: &Option<RRType>, param_ty: &Type) -> std::result::Result<(), ()> {
                    if let Some(base_ty) = base_ty {
                        if *base_ty.borrow() != *param_ty {
                            let base_ty = base_ty.borrow();
                            let base_ty = if let Type::Class(.., b, _) = &*base_ty { b } else { unreachable!() };
                            check_base(base_ty, param_ty)
                        } else {
                            Ok(())
                        }
                    } else {
                        Err(())
                    }
                }
                check_base(base_ty, param_ty)
            }
            _ => Err(())
        }
    }
    *p.ret_address.borrow_mut() = true;
    *p.consume.borrow_mut() = false;
    let parent_ty = gen_il(expr, st, p)?;
    let parent_ty = parent_ty.borrow();
    *p.consume.borrow_mut() = true;
    *p.ret_address.borrow_mut() = false;
    match &*parent_ty {
        Type::Class(_, _, ref path, ref cl_name, _, _) => {
            let ns = p.namespace.borrow();
            let ns = ns.find(path).unwrap();
            let cl = ns.find_class(|_|true, cl_name).unwrap();
            // TODO: 継承元のimplも確認
            fn find_func_recursive<'a, 'b>(ns: &'b NameSpace<'a>, base: &Option<RRType>, ident: &str) -> Option<Rc<Function<'a>>> {
                if let Some(base) = base {
                    if let Type::Class(.., name, base, _) = &*base.borrow() {
                        if let Some(func) = ns
                            .find_impl(name)
                            .and_then(|im| im.functions.find(ident).map(Rc::clone))
                            .and_then(|f| (!f.is_static).then_some(f)) {
                                Some(func)
                        } else {
                            find_func_recursive(ns, base, ident)
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            let func = if let Some(func) = ns
                .find_impl(cl_name)
                .and_then(|im| im.functions.find(ident).map(Rc::clone))
                .and_then(|f| (!f.is_static).then_some(f))
            {
                func
            } else if let Some(func) = find_func_recursive(ns, &cl.borrow().base, ident) {
                func
            } else {
                unreachable!();
            };
            let symbol_table = func.symbol_table.borrow();
            let params = &symbol_table
                .objs
                .iter()
                .filter(|o| o.borrow().kind == ObjectKind::Param)
                .skip((!func.is_static) as usize)
                .collect::<Vec<_>>();
            for (arg, param) in args.into_iter().zip(params) {
                let token = arg.token;
                let arg_ty = gen_il(arg, st, p)?;
                let param = param.borrow();
                let param_ty = &param.ty.borrow();
                if check_type(&arg_ty.borrow(), param_ty).is_err() {
                    e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty.borrow());
                }
            }
            let params = params
                .iter()
                .map(|p|p.borrow().ty.borrow().to_ilstr())
                .collect::<Vec<String>>()
                .join(", ");
            p.push_il_text(format!("\tcall instance {} {}::'{}'({})", func.rettype.borrow().to_ilstr(), parent_ty.to_ilstr(), ident, params));
            Ok(func.rettype.clone())
        }
        // 仮実装
        Type::Numeric(Numeric::I32) if ident == "to_string" => {
            p.push_il_text(format!("\tcall instance string {}::ToString()", parent_ty.to_ilstr()));
            Ok(RRType::new(Type::String))
        }
        ty => {
            let message = format!("[compiler unimplemented!()] primitive type methods: {:?}", ty);
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            Err(())
        }
    }
}

fn gen_il_lambda<'a>(
    _current_token: &[Token],
    _st: &SymbolTable,
    p: &'a Program<'a>,
    ty: RRType,
    ident: &str,
    parent: &str,
) -> Result<RRType> {
    //println!("\tldc.i4.0");  // FIXME: EventHandlerの第一引数がよく分からない
    if let Some(func) = p.namespace.borrow().find_fn(parent) {
        if let Some(func) = func.local_funcs.iter().find(|f|f.name == ident) {
            let params = &func
                .symbol_table
                .borrow()
                .objs
                .iter()
                .filter(|o| o.borrow().kind == ObjectKind::Param)
                .skip((!func.is_static) as usize)
                .map(|p|p.borrow().ty.borrow().to_ilstr())
                .collect::<Vec<String>>()
                .join(", ");
            p.push_il_text(format!("\tldloc '<{parent}>nested_class'"));
            p.push_il_text(format!("\tldftn instance void '{}'/'<>c__DisplayClass0_0'::'{}'({})", p.name, ident, params));
            p.push_il_text("\tnewobj instance void [mscorlib]System.EventHandler::.ctor(object, native int)");
        } else {
            unreachable!();
        }
    } else {
        unimplemented!();
    }
    Ok(ty)
}

fn gen_il_struct<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, obj: Ref<Object>, field: Vec<Node>) -> Result<RRType> {
    fn check_type(then: &Type, els: &Type) -> Result<RRType> {
        match (then, els) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(RRType::new(els.clone())),
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(RRType::new(then.clone())),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if then == els => Ok(RRType::new(then.clone())),
            _ => Err(())
        }
    }
    let ns = p.namespace.borrow();
    let (ns, name) = if let Type::Class(ClassKind::Struct, _, path, name, ..) = &*obj.ty.borrow() {
        (ns.find(path).unwrap(), name.to_string())
    } else {
        unreachable!()
    };
    let cl = ns.find_class(|k|k==&ClassKind::Struct, &name).unwrap();
    p.push_il_text(format!("\tldloca {}", obj.offset));
    p.push_il_text(format!("\tinitobj {}", obj.ty.borrow()));
    for (field_expr, field_dec) in field.into_iter().zip(&cl.borrow().field.objs) {
        p.push_il_text(format!("\tldloca {}", obj.offset));
        let field_token = field_expr.token;
        let ty = gen_il(field_expr, st, p)?;
        if check_type(&ty.borrow(), &field_dec.borrow().ty.borrow()).is_err() {
            e0012(Rc::clone(&p.errors), (p.path, &p.lines, field_token), &field_dec.borrow().ty.borrow(), &ty.borrow());
        }
        p.push_il_text(format!("\tstfld {} {}::'{}'", field_dec.borrow().ty.borrow().to_ilstr(), obj.ty.borrow(), field_dec.borrow().name));
    }
    p.push_il_text(format!("\tldloc {}", obj.offset));
    Ok(obj.ty.clone())
}

fn gen_il_field<'a>(
    current_token: &[Token],
    st: &SymbolTable,
    p: &'a Program<'a>,
    expr: Node,
    ident: &str,
) -> Result<RRType> {
    *p.consume.borrow_mut() = false;
    let parent_ty = gen_il(expr, st, p)?;
    let parent_ty = parent_ty.borrow();
    *p.consume.borrow_mut() = true;
    let (path, parent_name, base, is_mutable) = match &*parent_ty {
        Type::_Self(path, name, is_mutable) => {
            (path.to_vec(), name.to_string(), None, *is_mutable)
        }
        Type::Class(_, _, path, name, base, is_mutable) => {
            (path.to_vec(), name.to_string(), base.clone(), *is_mutable)
        }
        Type::Ptr(ty) => {
            match &*ty.borrow() {
                Type::_Self(ref path, ref name, is_mutable) => {
                    (path.to_vec(), name.to_string(), None, *is_mutable)
                }
                _ => {
                    let message = format!("[compiler unimplemented!()] primitive type field: {:?}", ty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                    return Err(());
                }
            }
        }
        ty => {
            let message = format!("[compiler unimplemented!()] primitive type field: {:?}", ty);
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            return Err(());
        }
    };
    let ns = p.namespace.borrow();
    let ns = ns.find(&path).unwrap();
    if let Some(cl) = ns.find_class(|_|true, &parent_name) {
        if let Some(field) = cl.borrow().field.find(ident) {
            if let Type::Class(ClassKind::Class, ..) = *field.borrow().ty.borrow()  {
                p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
            } else if *p.ret_address.borrow() {
                if let Type::Class(ClassKind::Class | ClassKind::NestedClass(..), ..) = *field.borrow().ty.borrow() {
                    p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                } else {
                    p.push_il_text(format!("\tldflda {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                }
            } else {
                p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
            }
            if is_mutable {
                Ok(field.borrow().ty.clone().into_mutable())
            } else {
                Ok(field.borrow().ty.clone())
            }
        } else {
            // baseクラスから検索
            fn search_from_base(base_ty: &Option<RRType>, parent_ty: &Type, p: &Program, ident: &str, is_mutable: bool) -> Result<RRType> {
                if let Some(base_ty) = base_ty {
                    let ns = p.namespace.borrow();
                    let base_ty = base_ty.borrow();
                    let (path, name, base)  = if let Type::Class(_, _, p, n, b, ..) = &*base_ty { (p, n, b) } else { unreachable!() };
                    if let Some(ns) = ns.find(path) {
                        if let Some(cl) = ns.find_class(|_|true, name) {
                            if let Some(field) = cl.borrow().field.find(ident) {
                                if let Type::Class(ClassKind::Class, ..) = *field.borrow().ty.borrow()  {
                                    p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                                } else if *p.ret_address.borrow() {
                                    if let Type::Class(ClassKind::Class | ClassKind::NestedClass(..), ..) = *field.borrow().ty.borrow() {
                                        p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), base_ty.to_ilstr(), ident));
                                    } else {
                                        p.push_il_text(format!("\tldflda {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), base_ty.to_ilstr(), ident));
                                    }
                                } else {
                                    p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), base_ty.to_ilstr(), ident));
                                }
                                if is_mutable {
                                    return Ok(field.borrow().ty.clone().into_mutable());
                                } else {
                                    return Ok(field.borrow().ty.clone());
                                }
                            }
                        }
                    }
                    return search_from_base(base, parent_ty, p, ident, is_mutable);
                }
                Err(())
            }
            search_from_base(&base, &parent_ty, p, ident, is_mutable)
                .map_err(|()| e0015(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, &parent_name))
        }
    } else {
        unreachable!()
    }
}

fn gen_il_variable(_current_token: &[Token], _st: &SymbolTable, p: &Program, obj: Ref<Object>) -> Result<RRType> {
    if let Some(parent) = &obj.parent {
        // 変数からフィールドへ変化した特殊な変数
        let parent_ty = gen_il_variable(_current_token, _st, p, parent.borrow())?;
        let parent_ty = parent_ty.borrow();
        let ident = obj.name.to_string();
        if let Type::Class(_, _, ref path, ref parent_name, _base, _is_mutable) = &*parent_ty {
            let ns = p.namespace.borrow();
            let ns = ns.find(path).unwrap();
            if let Some(cl) = ns.find_class(|_|true, parent_name) {
                if let Some(field) = cl.borrow().field.find(&ident) {
                    if let Type::Class(ClassKind::Class, ..) = *field.borrow().ty.borrow()  {
                        p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                    } else if *p.ret_address.borrow() {
                        if let Type::Class(ClassKind::Class | ClassKind::NestedClass(..), ..) = *field.borrow().ty.borrow() {
                            p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        } else {
                            p.push_il_text(format!("\tldflda {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        }
                    } else {
                        p.push_il_text(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                    }
                } else {
                    unimplemented!();
                }
            } else {
                unimplemented!();
            }
        } else {
            unimplemented!();
        }
    } else {
        // 普通の変数
        if obj.is_param() {
            p.push_il_text(format!("\tldarg {}", obj.offset));
        } else if *p.ret_address.borrow() {
            if let Type::Class(ClassKind::Class | ClassKind::NestedClass(..), ..) = *obj.ty.borrow() {
                p.push_il_text(format!("\tldloc {}", obj.offset));
            } else {
                p.push_il_text(format!("\tldloca {}", obj.offset));
            }
        } else {
            p.push_il_text(format!("\tldloc {}", obj.offset));
        }
    }
    if obj.is_mutable() {
        Ok(obj.ty.clone().into_mutable())
    } else {
        Ok(obj.ty.clone())
    }
}

fn gen_il_enum(current_token: &[Token], _st: &SymbolTable, p: &Program, obj: EnumObject) -> Result<RRType> {
    // TODO: 名前空間指定なしのenum
    e0007(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
    Err(())
}

fn gen_il_block<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, stmts: Vec<Node>) -> Result<RRType> {
    let mut ty = RRType::new(Type::Void);
    for stmt in stmts {
        match stmt.kind {
            NodeKind::Return { .. } => {
                ty = gen_il(stmt, st, p)?;
                break;
            }
            _ => {
                ty = gen_il(stmt, st, p)?;
            }
        }
    }
    Ok(ty)
}

fn gen_il_if<'a>(current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, cond: Node, then: Node, els: Option<Box<Node>>) -> Result<RRType> {
    fn check_type(then: &Type, els: &Type) -> Result<RRType> {
        match (then, els) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(RRType::new(els.clone())),
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(RRType::new(then.clone())),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if then == els => Ok(RRType::new(then.clone())),
            _ => Err(())
        }
    }
    let token = cond.token;
    let cond_type = gen_il(cond, st, p)?;
    if *cond_type.borrow() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.borrow());
    }
    let seq = label_seq();
    let else_label = format!("IL_else{}", seq);
    let end_label = format!("IL_end{}", seq);
    p.push_il_text(format!("\tbrfalse {}", else_label));
    let then_type = gen_il(then, st, p)?;
    p.push_il_text(format!("\tbr {}", end_label));
    p.push_il_text(format!("{}:", else_label));
    let els = els.map(|els| (els.token, gen_il(*els, st, p)));
    p.push_il_text(format!("{}:", end_label));
    if let Some(els) = els {
        let els_token = els.0;
        let els_type = els.1?;
        let els_type = els_type.borrow();
        check_type(&then_type.borrow(), &els_type)
            .map_err(|()| {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, els_token), &then_type.borrow(), &els_type);
            })
    } else if *then_type.borrow() != Type::Void {
        e0018(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &then_type.borrow());
        Err(())
    } else {
        Ok(then_type)
    }
}

fn gen_il_while<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, cond: Node, then: Node, brk_label_seq: usize) -> Result<RRType> {
    let begin_label = format!("IL_begin{}", label_seq());
    let end_label = format!("IL_break{}", brk_label_seq);
    p.push_il_text(format!("{}:", begin_label));
    let token = cond.token;
    let cond_type = gen_il(cond, st, p)?;
    if *cond_type.borrow() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.borrow());
    }
    p.push_il_text(format!("\tbrfalse {}", end_label));
    let then_token = then.token;
    let then_type = gen_il(then, st, p)?;
    if !matches!(*then_type.borrow(), /* TODO: Type::Never(_) | */ Type::Void) {
        let message = format!("[compiler unimplemented!()] expect `Type::Void`, found {}", then_type.borrow());
        warning(Rc::clone(&p.errors), (p.path, &p.lines, then_token), &message);
    }
    p.push_il_text(format!("\tbr {}", begin_label));
    p.push_il_text(format!("{}:", end_label));
    Ok(then_type)
}

fn gen_il_loop<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, then: Node, brk_label_seq: usize) -> Result<RRType> {
    let begin_label = format!("IL_begin{}", label_seq());
    let end_label = format!("IL_break{}", brk_label_seq);
    p.push_il_text(format!("{}:", begin_label));
    let then_token = then.token;
    let then_type = gen_il(then, st, p)?;
    p.push_il_text(format!("\tbr {}", begin_label));
    p.push_il_text(format!("{}:", end_label));
    if !matches!(*then_type.borrow(), /* TODO: Type::Never(_) | */ Type::Void) {
        let message = format!("[compiler unimplemented!()] expect `Type::Void`, found {}", then_type.borrow());
        warning(Rc::clone(&p.errors), (p.path, &p.lines, then_token), &message);
    }
    Ok(then_type)
}

fn gen_il_assign<'a>(current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, lhs: Node, rhs: Node) -> Result<RRType> {
    fn check_type(lty: &Type, rty: &Type) -> Result<()> {
        match (lty, rty) {
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if lty == rty => Ok(()),
            _ => Err(())
        }
    }
    match lhs.kind {
        NodeKind::Variable { obj } => {
            if let Some(parent) = &obj.borrow().parent {
                let ident = obj.borrow().name.to_string();
                *p.ret_address.borrow_mut() = true;
                let parent_ty = gen_il_variable(current_token, st, p, parent.borrow())?;
                let parent_ty = parent_ty.borrow();
                *p.ret_address.borrow_mut() = false;
                match &*parent_ty {
                    Type::Class(_, _, ref path, ref name, _, _) => {
                        let ns = p.namespace.borrow();
                        let ns = ns.find(path).unwrap();
                        if let Some(cl) = ns.find_class(|_|true, name) {
                            if let Some(field) = cl.borrow().field.find(&ident) {
                                let rty = gen_il(rhs, st, p)?;
                                if check_type(&field.borrow().ty.borrow(), &rty.borrow()).is_err() {
                                    e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.borrow(), &rty.borrow());
                                }
                                p.push_il_text(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                            }
                        }
                    }
                    ty => {
                        let message = format!("[compiler unimplemented!()] primitive type: {:?}", ty);
                        e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                    }
                }
                return Ok(RRType::new(Type::Void));
            }
            let rty = gen_il(rhs, st, p)?;
            if check_type(&obj.borrow().ty.borrow(), &rty.borrow()).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().ty.borrow(), &rty.borrow());
            }
            if obj.borrow().is_param() {
                p.push_il_text(format!("\tstarg {}", obj.borrow().offset));
            } else {
                p.push_il_text(format!("\tstloc {}", obj.borrow().offset));
            }
        }
        NodeKind::UnaryOp { kind: UnaryOpKind::Deref, expr } => {
            let lty = gen_il(*expr, st, p)?;
            let rty = gen_il(rhs, st, p)?;
            let lty = match &*lty.borrow() {
                Type::Ptr(lty) => lty.borrow().clone(),
                _ => unreachable!()
            };
            if check_type(&lty, &rty.borrow()).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty, &rty.borrow());
            }
            match lty {
                Type::Ptr(_) => p.push_il_text("\tstind.i"),
                Type::Numeric(Numeric::I32) => p.push_il_text("\tstind.i4"),
                _ => {
                    let message = format!("[compiler unimplemented!()] dereference {:?}", lty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                }
            }
        }
        NodeKind::Field { expr, ident } => {
            *p.ret_address.borrow_mut() = true;
            *p.consume.borrow_mut() = false;
            let parent_ty = gen_il(*expr, st, p)?;
            let parent_ty = parent_ty.borrow();
            *p.consume.borrow_mut() = true;
            *p.ret_address.borrow_mut() = false;
            match &*parent_ty {
                Type::_Self(ref path, ref name, _)          |
                Type::Class(_, _, ref path, ref name, _, _) => {
                    let ns = p.namespace.borrow();
                    let ns = ns.find(path).unwrap();
                    if let Some(cl) = ns.find_class(|_|true, name) {
                        if let Some(field) = cl.borrow().field.find(&ident) {
                            let rty = gen_il(rhs, st, p)?;
                            if check_type(&field.borrow().ty.borrow(), &rty.borrow()).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.borrow(), &rty.borrow());
                            }
                            p.push_il_text(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        }
                    }
                }
                ty => {
                    let message = format!("[compiler unimplemented!()] primitive type field: {:?}", ty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                }
            }
        }
        _ => unreachable!()
    }
    Ok(RRType::new(Type::Void))
}

fn gen_il_return<'a>(current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, expr: Option<Box<Node>>, func_retty: RRType) -> Result<RRType> {
    fn check_type(arg: &Type, param: &Type) -> Result<()> {
        match (arg, param) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if arg == param => Ok(()),
            _ => Err(())
        }
    }
    let rettype = if let Some(expr) = expr {
        gen_il(*expr, st, p)?
    } else {
        RRType::new(Type::Void)
    };
    if check_type(&rettype.borrow(), &func_retty.borrow()).is_err() {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &func_retty.borrow(), &rettype.borrow());
    }

    debug_assert_ne!(&*rettype.borrow(), &Type::Numeric(Numeric::Integer));

    p.push_il_text("\tret");
    // TODO: Type::Never(rettype)
    Ok(rettype)
}

fn gen_il_break(_current_token: &[Token], _st: &SymbolTable, p: &Program, brk_label_seq: usize) -> Result<RRType> {
    p.push_il_text(format!("\tbr IL_break{}", brk_label_seq));
    // TODO: Type::Never(rettype)
    Ok(RRType::new(Type::Void))
}

fn gen_il_cast<'a>(current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, new_type: RRType, expr: Node) -> Result<RRType> {
    let old_type = gen_il(expr, st, p)?;
    let old_type = old_type.borrow();
    match &*new_type.borrow() {
        Type::Numeric(Numeric::I32) => {
            match *old_type {
                Type::Numeric(..) | Type::Float(..) | Type::Enum(..) | Type::Bool | Type::Char => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Numeric(Numeric::I32)),
            }
            p.push_il_text("\tconv.i4");
        }
        Type::Float(Float::F32) => {
            match *old_type {
                Type::Numeric(..) | Type::Float(Float::F32) => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Float(Float::F32)),
            }
            p.push_il_text("\tconv.r4");
        }
        Type::Bool => {
            match *old_type {
                Type::Bool => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Bool),
            }
            p.push_il_text("\tldc.i4.0");
            p.push_il_text("\tcgt");
        }
        Type::Char => {
            match *old_type {
                Type::Char | Type::Numeric(_) => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Char),
            }
            p.push_il_text("\tconv.u2");
        }
        Type::Ptr(_) => {
            todo!("cast to ref type");
        }
        Type::Void => unreachable!(),
        t => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), t)
    }
    Ok(new_type)
}

fn gen_il_unaryop<'a>(current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, kind: UnaryOpKind, expr: Node) -> Result<RRType> {
    match kind {
        UnaryOpKind::Not => {
            let ty = gen_il(expr, st, p)?;
            match &*ty.borrow() {
                Type::Bool => {
                    p.push_il_text("\tldc.i4.0");
                    p.push_il_text("\tceq");
                }
                Type::Numeric(_) => {
                    p.push_il_text("\tnot");
                }
                _ => unreachable!()
            }
            Ok(ty)
        }
        UnaryOpKind::Neg => {
            let ty= gen_il(expr, st, p)?;
            if let Type::Numeric(..) | Type::Float(..) = *ty.borrow() {
                p.push_il_text("\tneg");
            } else {
                unreachable!()
            }
            Ok(ty)
        }
        UnaryOpKind::Ref => {
            if let NodeKind::Variable { obj } = expr.kind {
                let obj = obj.borrow();
                if obj.is_param() {
                    p.push_il_text(format!("\tldarga {}", obj.offset));
                } else {
                    p.push_il_text(format!("\tldloca {}", obj.offset));
                }
                Ok(RRType::new(Type::Ptr(obj.ty.clone())))
            } else {
                Ok(RRType::new(Type::Ptr(gen_il(expr, st, p)?)))
            }
        }
        UnaryOpKind::Deref => {
            let ret_address = *p.ret_address.borrow();
            *p.ret_address.borrow_mut() = false;
            *p.consume.borrow_mut() = false;
            let ty = gen_il(expr, st, p)?;
            let ty = ty.borrow();
            *p.consume.borrow_mut() = true;
            *p.ret_address.borrow_mut() = ret_address;
            match &*ty {
                Type::Ptr(ty) => {
                    match &*ty.borrow() {
                        Type::Ptr(_) => p.push_il_text("\tldind.i"),
                        Type::Numeric(Numeric::I32) => p.push_il_text("\tldind.i4"),
                        Type::Float(Float::F32) => p.push_il_text("\tldind.r4"),
                        ty => {
                            let message = format!("[compiler unimplemented!()] dereferenced {:?}", ty);
                            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                        }
                    }
                    Ok(ty.clone())
                }
                Type::Box(ty) => {
                    match &*ty.borrow() {
                        ty@Type::Class(ClassKind::Struct, ..) => p.push_il_text(format!("\tunbox {}", ty.to_ilstr())),
                        ty => p.push_il_text(format!("\tunbox.any {}", ty.to_ilstr())),
                    }
                    Ok(ty.clone())
                }
                _ => unreachable!()
            }
        }
    }
}

fn gen_il_binaryop<'a>(current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, kind: BinaryOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    let ltype = gen_il(lhs, st, p)?;
    let ltype = ltype.borrow();
    let rtype = gen_il(rhs, st, p)?;
    let rtype = rtype.borrow();
    let mut is_bool = false;
    match &*ltype {
        Type::Numeric(..) => match kind {
            BinaryOpKind::Add    => p.push_il_text("\tadd"),
            BinaryOpKind::Sub    => p.push_il_text("\tsub"),
            BinaryOpKind::Mul    => p.push_il_text("\tmul"),
            BinaryOpKind::Div    => p.push_il_text("\tdiv"),
            BinaryOpKind::Rem    => p.push_il_text("\trem"),
            BinaryOpKind::BitXor => p.push_il_text("\txor"),
            BinaryOpKind::BitAnd => p.push_il_text("\tand"),
            BinaryOpKind::BitOr  => p.push_il_text("\tor"),
            BinaryOpKind::Shl    => p.push_il_text("\tshl"),
            BinaryOpKind::Shr    => p.push_il_text("\tshr"),

            BinaryOpKind::Eq => {
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                p.push_il_text("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                p.push_il_text("\tcgt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                p.push_il_text("\tceq");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                p.push_il_text("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                p.push_il_text("\tclt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
        }
        Type::Float(..) => match kind {
            BinaryOpKind::Add => p.push_il_text("\tadd"),
            BinaryOpKind::Sub => p.push_il_text("\tsub"),
            BinaryOpKind::Mul => p.push_il_text("\tmul"),
            BinaryOpKind::Div => p.push_il_text("\tdiv"),
            BinaryOpKind::Rem => p.push_il_text("\trem"),

            BinaryOpKind::Eq => {
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                p.push_il_text("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                p.push_il_text("\tcgt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                p.push_il_text("\tceq");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                p.push_il_text("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                p.push_il_text("\tclt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            _ => {
                e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
        }
        Type::Char | Type::Bool => match kind {
            BinaryOpKind::Add |
            BinaryOpKind::Sub |
            BinaryOpKind::Mul |
            BinaryOpKind::Div |
            BinaryOpKind::Rem => {
                e0023(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
            BinaryOpKind::Eq => {
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                p.push_il_text("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                p.push_il_text("\tcgt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                p.push_il_text("\tceq");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                p.push_il_text("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                p.push_il_text("\tclt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            _ => {
                e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
        }
        Type::String => match kind {
            BinaryOpKind::Add => {
                p.push_il_text("\tcall string System.String::Concat(string, string)");
            }
            BinaryOpKind::Sub |
            BinaryOpKind::Mul |
            BinaryOpKind::Div |
            BinaryOpKind::Rem => {
                e0023(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
            BinaryOpKind::Eq => {
                p.push_il_text("\tcall bool System.String::op_Equality(string, string)");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                p.push_il_text("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                p.push_il_text("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tcgt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                p.push_il_text("call bool System.String::op_Inequality(string, string)");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                p.push_il_text("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                p.push_il_text("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tclt");
                p.push_il_text("\tldc.i4.0");
                p.push_il_text("\tceq");
                is_bool = true;
            }
            _ => {
                e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
        }
        _ => {
            e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
            return Err(());
        }
    }
    match (&*ltype, &*rtype) {
        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => {
            if is_bool {
                Ok(RRType::new(Type::Bool))
            } else {
                Ok(RRType::new(rtype.clone()))
            }
        }
        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => {
            if is_bool {
                Ok(RRType::new(Type::Bool))
            } else {
                Ok(RRType::new(ltype.clone()))
            }
        }
        _ if *ltype == *rtype => {
            if is_bool {
                Ok(RRType::new(Type::Bool))
            } else {
                Ok(RRType::new(ltype.clone()))
            }
        }
        _ => {
            e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ltype, &rtype);
            Err(())
        }
    }
}

fn gen_il_shortcircuitop<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    let end_label  = format!("IL_end{}", label_seq());
    match kind {
        ShortCircuitOpKind::And => {
            p.push_il_text("\tldc.i4.0");
            let token = lhs.token;
            let ltype = gen_il(lhs, st, p)?;
            if *ltype.borrow() != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype.borrow());
            }
            p.push_il_text(format!("\tbrfalse {}", end_label));
            p.push_il_text("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, st, p)?;
            if *rtype.borrow() != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype.borrow());
            }
            p.push_il_text(format!("{}:", end_label));
        }
        ShortCircuitOpKind::Or => {
            p.push_il_text("\tldc.i4.1");
            let token = lhs.token;
            let ltype = gen_il(lhs, st, p)?;
            if *ltype.borrow() != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype.borrow());
            }
            p.push_il_text(format!("\tbrtrue {}", end_label));
            p.push_il_text("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, st, p)?;
            if *rtype.borrow() != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype.borrow());
            }
            p.push_il_text(format!("{}:", end_label));
        }
    }
    Ok(RRType::new(Type::Bool))
}

fn gen_il_semi<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, expr: Node) -> Result<RRType> {
    let ty = gen_il(expr, st, p)?;
    if *ty.borrow() != Type::Void {
        p.push_il_text("\tpop");
    }
    Ok(RRType::new(Type::Void))
}

fn gen_il_path<'a>(_current_token: &[Token], st: &SymbolTable, p: &'a Program<'a>, segment: &str, mut full_path: Vec<String>, child: Node) -> Result<RRType> {
    fn check_type(arg: &Type, param: &Type) -> Result<()> {
        match (arg, param) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if arg == param => Ok(()),
            _ => Err(())
        }
    }
    match child.kind {
        NodeKind::Path { segment, child } => {
            full_path.push(segment.to_string());
            gen_il_path(_current_token, st, p, &segment, full_path, *child)
        }
        NodeKind::Call { name, args } => {
            let ns = p.namespace.borrow();
            let ns = ns.find(&full_path).unwrap();
            if let Some(func) = ns.find_fn(&name) {
                let objs = &func
                    .symbol_table
                    .borrow()
                    .objs;
                let params = objs
                    .iter()
                    .filter(|o| o.borrow().kind == ObjectKind::Param)
                    .collect::<Vec<_>>();
                for (arg, param) in args.into_iter().zip(&params) {
                    let token = arg.token;
                    let param = param.borrow();
                    let param_ty = &param.ty.borrow();
                    let arg_ty = gen_il(arg, st, p)?;
                    if check_type(&arg_ty.borrow(), param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty.borrow());
                    }
                }
                let params = params
                    .iter()
                    .map(|p|p.borrow().ty.borrow().to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                p.push_il_text(format!("\tcall {} '{}'::'{}'({})", func.rettype.borrow().to_ilstr(), p.name, name, params));
                Ok(func.rettype.clone())
            } else if let Some(im) = ns.find_impl(full_path.last().unwrap()) {
                let func = im
                    .functions
                    .iter()
                    .find(|f|f.name==name)
                    .unwrap();
                let objs = &func
                    .symbol_table
                    .borrow()
                    .objs;
                let params = objs
                    .iter()
                    .filter(|o| o.borrow().kind == ObjectKind::Param)
                    .collect::<Vec<_>>();
                for (arg, param) in args.into_iter().zip(&params) {
                    let token = arg.token;
                    let param = param.borrow();
                    let param_ty = &param.ty.borrow();
                    let arg_ty = gen_il(arg, st, p)?;
                    if check_type(&arg_ty.borrow(), param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty.borrow());
                    }
                }
                let params = params
                    .iter()
                    .skip((!func.is_static) as usize)
                    .map(|o|o.borrow().ty.borrow().to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                if ns.is_foreign {
                    let reference = &im.reference.as_ref().unwrap();
                    if func.is_ctor {
                        p.push_il_text(format!("\tnewobj instance void {}::'{}'({})", func.rettype.borrow().to_ilstr(), name, params));
                    } else {
                        p.push_il_text(format!("\tcall {} [{}]{}::'{}'({})", func.rettype.borrow().to_ilstr(), reference, full_path.join("."), name, params));
                    }
                } else {
                    p.push_il_text(format!("\tcall {} {}::'{}'({})", func.rettype.borrow().to_ilstr(), segment, name, params));
                }
                Ok(func.rettype.clone())
            } else {
                unreachable!()
            }
        }
        NodeKind::Enum { obj } => {
            let ns = p.namespace.borrow();
            let ns = ns.find(&full_path[..full_path.len()-1]).unwrap();
            let ed = ns.find_enum(full_path.last().unwrap()).unwrap();
            let field = ed.fields.iter().find(|f| f.name == obj.name).unwrap();
            p.push_il_text(format!("\tldc.i4 {}", field.value));
            // TODO: Ok(obj.ty)
            Ok(RRType::new(Type::Enum(None, full_path[..full_path.len()-1].to_vec(), full_path.last().unwrap().to_string())))
        }
        _ => {
            gen_il(child, st, p)
        }
    }
}

fn gen_il_empty() -> Result<RRType> {
    Ok(RRType::new(Type::Void))
}
