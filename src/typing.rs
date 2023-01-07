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
use crate::utils::remove_seq;
use std::rc::Rc;
use std::cell::{Ref, RefCell};

type Result<T> = std::result::Result<T, ()>;

/// 型検査
/// アフィン型システム
/// 型推論
pub fn typing<'a>(node: Node, st: &mut SymbolTable, p: &'a Program<'a>) -> Result<RRType> {
    match node.kind {
        NodeKind::Integer { ty, num } => {
            typing_integer(node.token, st, p, ty, num)
        }
        NodeKind::Float { ty, num } => {
            typing_float(node.token, st, p, ty, num)
        }
        NodeKind::String { ty, str } => {
            typing_string(node.token, st, p, ty, &str)
        }
        NodeKind::Box { method } => {
            typing_box(node.token, st, p, *method)
        }
        NodeKind::Builtin { kind, args } => {
            // TODO:
            //typing_builtin(node.token, st, kind, args, p)
            Ok(RRType::new(Type::Void))
        }
        NodeKind::Let { obj } => {
            typing_let(node.token, st, p, obj.clone())
        }
        NodeKind::Call { name, args } => {
            typing_call(node.token, st, p, &name, args)
        }
        NodeKind::Method { expr, ident, args } => {
            typing_method(node.token, st, p, *expr, &ident, args)
        }
        NodeKind::Lambda { ty, ident, parent } => {
            typing_lambda(node.token, st, p, ty, &ident, &parent)
        }
        NodeKind::Struct { obj, field } => {
            typing_struct(node.token, st, p, obj.borrow(), field)
        }
        NodeKind::Field { expr, ident } => {
            typing_field(node.token, st, p, *expr, &ident)
        }
        NodeKind::Variable { obj } => {
            typing_variable(node.token, st, p, obj.clone())
        }
        NodeKind::Enum { obj } => {
            typing_enum(node.token, st, p, obj)
        }
        NodeKind::Block { stmts } => {
            typing_block(node.token, st, p, stmts)
        }
        NodeKind::If { cond, then, els } => {
            typing_if(node.token, st, p, *cond, *then, els)
        }
        NodeKind::While { cond, then, brk_label_seq } => {
            typing_while(node.token, st, p, *cond, *then, brk_label_seq)
        }
        NodeKind::Loop { then, brk_label_seq } => {
            typing_loop(node.token, st, p, *then, brk_label_seq)
        }
        NodeKind::Assign { lhs, rhs } => {
            typing_assign(node.token, st, p, *lhs, *rhs)
        }
        NodeKind::Return { expr, func_retty } => {
            typing_return(node.token, st, p, expr, func_retty)
        }
        NodeKind::Break { brk_label_seq } => {
            typing_break(node.token, st, p, brk_label_seq)
        }
        NodeKind::Cast { ty: new_type, expr } => {
            typing_cast(node.token, st, p, new_type, *expr)
        }
        NodeKind::UnaryOp { kind, expr } => {
            typing_unaryop(node.token, st, p, kind, *expr)
        }
        NodeKind::BinaryOp { kind, lhs, rhs } => {
            typing_binaryop(node.token, st, p, kind, *lhs, *rhs)
        }
        NodeKind::ShortCircuitOp { kind, lhs, rhs } => {
            typing_shortcircuitop(node.token, st, p, kind, *lhs, *rhs)
        }
        NodeKind::Semi { expr } => {
            typing_semi(node.token, st, p, *expr)
        }
        NodeKind::Path { segment, child } => {
            typing_path(node.token, st, p, &segment, vec![segment.to_string()], *child)
        }
        NodeKind::Empty => {
            typing_empty()
        }
    }
}

fn typing_integer(current_token: &[Token], st: &mut SymbolTable, p: &Program, ty: RRType, num: i128) -> Result<RRType> {
    Ok(ty)
}

fn typing_float(_current_token: &[Token], st: &mut SymbolTable, p: &Program, ty: RRType, num: FloatNum) -> Result<RRType> {
    Ok(ty)
}

fn typing_string(_current_token: &[Token], st: &mut SymbolTable, p: &Program, ty: RRType, str: &str) -> Result<RRType> {
    Ok(ty)
}

fn typing_let(current_token: &[Token], st: &mut SymbolTable, p: &Program, obj: Rc<RefCell<Object>>) -> Result<RRType> {
    st.push(obj.clone());
    typing_variable(current_token, st, p, obj)
}

fn typing_box<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, method: Node) -> Result<RRType> {
    if let NodeKind::Call { name, args } = method.kind {
        match name.as_str() {
            "new" => {
                if args.len() != 1 {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, method.token), 1, args.len());
                }
                let boxed_ty = gen_il(args.into_iter().next().unwrap(), p)?;
                //println!("\tbox [System.Runtime]System.Int32");
                p.push_il_text(format!("\tbox {}", boxed_ty.to_ilstr()));
                Ok(Type::Box(RRType::new(boxed_ty)))
            }
            _ => {
                e0014(Rc::clone(&p.errors), (p.path, &p.lines, method.token), &name, "Box");
                Err(())
            }
        }
    } else {
        e0003(Rc::clone(&p.errors), (p.path, &p.lines, method.token));
        Err(())
    }
}
fn typing_call<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, name: &str, args: Vec<Node>) -> Result<RRType> {
    fn check_type(arg: &Type, param: &Type) -> Result<()> {
        match (arg, param) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if arg == param => Ok(()),
            _ => Err(())
        }
    }
    if let Some(func) = p.namespace.borrow().find_fn(name) {
        let objs = &func
            .symbol_table
            .borrow()
            .objs;
        let params = objs
            .iter()
            .filter(|o| o.borrow().kind == ObjectKind::Param)
            .collect::<Vec<_>>();
        if params.len() != args.len() {
            e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
        }
        for (arg, param) in args.into_iter().zip(&params) {
            let token = arg.token;
            let param = param.borrow();
            let param_ty = &param.ty.borrow();
            let arg_ty = gen_il(arg, p)?;
            if check_type(&arg_ty, param_ty).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty);
            }
        }
        let params = params
            .iter()
            .map(|p|p.borrow().ty.borrow().to_ilstr())
            .collect::<Vec<String>>()
            .join(", ");
        p.push_il_text(format!("\tcall {} '{}'::'{}'({})", func.rettype.borrow().to_ilstr(), p.name, name, params));
        Ok(func.rettype.borrow().clone())
    } else {
        e0013(Rc::clone(&p.errors), (p.path, &p.lines, current_token), name);
        Err(())
    }
}

fn typing_method<'a>(
    current_token: &[Token],
    st: &mut SymbolTable,
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
    let parent_ty = gen_il(expr, p)?;
    *p.ret_address.borrow_mut() = false;
    match parent_ty {
        Type::Class(_, _, ref path, ref cl_name, _, _) => {
            let ns = p.namespace.borrow();
            let ns = if let Some(ns) = ns.find(path) {
                ns
            } else {
                let message = format!("failed to resolve: use of undeclared type `{}`", path.join("::"));
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                return Err(());
            };
            if let Some(cl) = ns.find_class(|_|true, cl_name) {
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
                    e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, cl_name);
                    return Err(());
                };
                let symbol_table = func.symbol_table.borrow();
                let params = &symbol_table
                    .objs
                    .iter()
                    .filter(|o| o.borrow().kind == ObjectKind::Param)
                    .skip((!func.is_static) as usize)
                    .collect::<Vec<_>>();
                if params.len() != args.len() {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
                }
                for (arg, param) in args.into_iter().zip(params) {
                    let token = arg.token;
                    let arg_ty = gen_il(arg, p)?;
                    let param = param.borrow();
                    let param_ty = &param.ty.borrow();
                    if check_type(&arg_ty, param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty);
                    }
                }
                let params = params
                    .iter()
                    .map(|p|p.borrow().ty.borrow().to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                p.push_il_text(format!("\tcall instance {} {}::'{}'({})", func.rettype.borrow().to_ilstr(), parent_ty.to_ilstr(), ident, params));
                let x = Ok(func.rettype.borrow().clone());
                x
            } else {
                e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, cl_name);
                Err(())
            }
        }
        // 仮実装
        Type::Numeric(Numeric::I32) if ident == "to_string" => {
            p.push_il_text(format!("\tcall instance string {}::ToString()", parent_ty.to_ilstr()));
            Ok(Type::String)
        }
        ty => {
            let message = format!("[compiler unimplemented!()] primitive type methods: {:?}", ty);
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            Err(())
        }
    }
}

fn typing_lambda<'a>(
    _current_token: &[Token],
    st: &mut SymbolTable,
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
            p.push_il_text(format!("\tldloc '{}'", "<main>nested_class"));
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

fn typing_struct<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, obj: Ref<Object>, field: Vec<Node>) -> Result<RRType> {
    let ns = p.namespace.borrow();
    let (ns, name) = if let Type::Class(ClassKind::Struct, _, path, name, ..) = &*obj.ty.borrow() {
        if let Some(ns) = ns.find(path) {
            (ns, name.to_string())
        } else {
            let message = format!("failed to resolve: use of undeclared type `{}`", path.join("::"));
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            return Err(());
        }
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &remove_seq(&obj.name));
        return Err(());
    };
    if let Some(st) = ns.find_class(|k|k==&ClassKind::Struct, &name) {
        if field.len() != st.borrow().field.objs.len() {
            e0017(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &st.borrow().name);
        }
        p.push_il_text(format!("\tldloca {}", obj.offset));
        p.push_il_text(format!("\tinitobj {}", obj.ty.borrow()));
        for (field_expr, field_dec) in field.into_iter().zip(&st.borrow().field.objs) {
            p.push_il_text(format!("\tldloca {}", obj.offset));
            gen_il(field_expr, p)?;
            p.push_il_text(format!("\tstfld {} {}::'{}'", field_dec.borrow().ty.borrow().to_ilstr(), obj.ty.borrow(), field_dec.borrow().name));
        }
        p.push_il_text(format!("\tldloc {}", obj.offset));
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &remove_seq(&obj.name));
    }
    Ok(obj.ty.borrow().clone())
}

fn typing_field<'a>(
    current_token: &[Token],
    st: &mut SymbolTable,
    p: &'a Program<'a>,
    expr: Node,
    ident: &str,
) -> Result<RRType> {
    let parent_ty = gen_il(expr, p)?;
    let (path, parent_name, base, is_mutable) = match parent_ty.clone() {
        Type::_Self(path, name, is_mutable) => {
            //println!("\tldarg.0");
            (path, name, None, is_mutable)
        }
        Type::Class(_, _, path, name, base, is_mutable) => {
            (path, name, base, is_mutable)
        }
        Type::Ptr(ty) => {
            match &*ty.borrow() {
                Type::_Self(path, name, is_mutable) => {
                    // &self
                    // tmp
                    //println!("\tldarg.0");
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
    let ns = if let Some(ns) = ns.find(&path) {
        ns
    } else {
        let message = format!("failed to resolve: use of undeclared type `{}`", path.join("::"));
        e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
        return Err(());
    };
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
                Ok(field.borrow().ty.borrow().clone().into_mutable())
            } else {
                Ok(field.borrow().ty.borrow().clone())
            }
        } else {
            // baseクラスから検索
            fn search_from_base(base_ty: Option<RRType>, parent_ty: &Type, p: &Program, ident: &str, is_mutable: bool) -> Result<Type> {
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
                                    return Ok(field.borrow().ty.borrow().clone().into_mutable());
                                } else {
                                    return Ok(field.borrow().ty.borrow().clone());
                                }
                            }
                        }
                    }
                    return search_from_base(base.clone(), parent_ty, p, ident, is_mutable);
                }
                Err(())
            }
            search_from_base(base, &parent_ty, p, ident, is_mutable)
                .map_err(|()| e0015(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, &parent_name))
        }
    } else {
        // unreachable?
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &parent_name);
        Err(())
    }
}

fn typing_variable(current_token: &[Token], st: &mut SymbolTable, p: &Program, obj: Rc<RefCell<Object>>) -> Result<RRType> {
    if let Some(obj) = st.find_mut(&obj.borrow().name) {
        if obj.borrow().used {
            let message = format!("use of moved value: `{}`", obj.borrow().name);
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            Err(())
        } else {
            if !obj.borrow().assigned {
                e0027(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().name);
            }
            obj.borrow_mut().consume();
            Ok(obj.borrow().ty.clone())
        }
    } else {
        e0007(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().name);
        Err(())
    }
}

fn typing_enum(current_token: &[Token], st: &mut SymbolTable, p: &Program, obj: EnumObject) -> Result<RRType> {
    // TODO: 名前空間指定なしのenum
    e0007(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
    Err(())
}

fn typing_block<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, stmts: Vec<Node>) -> Result<RRType> {
    let mut ty = RRType::new(Type::Void);
    st.enter_scope();
    for stmt in stmts {
        match stmt.kind {
            NodeKind::Return { .. } => {
                ty = typing(stmt, st, p)?;
                break;
            }
            _ => {
                ty = typing(stmt, st, p)?;
            }
        }
    }
    st.leave_scope();
    Ok(ty)
}

fn typing_if<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, cond: Node, then: Node, els: Option<Box<Node>>) -> Result<RRType> {
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
    let cond_type = typing(cond, st, p)?;
    if *cond_type.borrow() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.borrow());
    }
    let then_type = typing(then, st, p)?;
    let els = els.map(|els| (els.token, typing(*els, st, p)));
    if let Some(els) = els {
        let els_token = els.0;
        let els_type = els.1?;
        check_type(&then_type.borrow(), &els_type.borrow())
            .map_err(|()| {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, els_token), &then_type.borrow(), &els_type.borrow());
            })
    } else if *then_type.borrow() != Type::Void {
        e0018(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &then_type.borrow());
        Err(())
    } else {
        Ok(then_type)
    }
}

fn typing_while<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, cond: Node, then: Node, brk_label_seq: usize) -> Result<RRType> {
    let token = cond.token;
    let cond_type = typing(cond, st, p)?;
    if *cond_type.borrow() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.borrow());
    }
    let then_type = typing(then, st, p)?;
    if *then_type.borrow() != Type::Void {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Void, &then_type.borrow());
    }
    Ok(RRType::new(Type::Void))
}

fn typing_loop<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, then: Node, brk_label_seq: usize) -> Result<RRType> {
    let then_type = typing(then, st, p)?;
    if *then_type.borrow() != Type::Void {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Void, &then_type.borrow());
    }
    Ok(RRType::new(Type::Void))
}

fn typing_assign<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, lhs: Node, rhs: Node) -> Result<RRType> {
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
                let parent_ty = typing_variable(current_token, st, p, parent.clone())?;
                *p.ret_address.borrow_mut() = false;
                match parent_ty {
                    Type::Class(_, _, ref path, ref name, _, is_mutable) => {
                        let namespace = p.namespace.borrow();
                        let ns = if let Some(ns) = namespace.find(path) {
                            ns
                        } else {
                            let message = format!("failed to resolve: use of undeclared crate or module `{}`", path.join("::"));
                            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            return Err(());
                        };
                        if let Some(cl) = ns.find_class(|_|true, name) {
                            if let Some(field) = cl.borrow().field.find(&ident) {
                                let rty = gen_il(rhs, p)?;
                                if check_type(&field.borrow().ty.borrow(), &rty).is_err() {
                                    e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.borrow(), &rty);
                                }
                                if !is_mutable {
                                    let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                                }
                                p.push_il_text(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                            } else {
                                e0015(Rc::clone(&p.errors), (p.path, &p.lines, lhs.token), &ident, name);
                            }
                        } else {
                            e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), name);
                        }
                    }
                    ty => {
                        let message = format!("[compiler unimplemented!()] primitive type: {:?}", ty);
                        e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                    }
                }
                return Ok(Type::Void);
            }
            let rty = gen_il(rhs, p)?;
            let is_assigned = obj.borrow().is_assigned();
            obj.borrow_mut().assigned = true;
            let obj = obj.borrow();
            if !obj.is_mutable() && is_assigned {
                e0028(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
            }
            if check_type(&obj.ty.borrow(), &rty).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.ty.borrow(), &rty);
            }
            if obj.is_param() {
                p.push_il_text(format!("\tstarg {}", obj.offset));
            } else {
                p.push_il_text(format!("\tstloc {}", obj.offset));
            }
        }
        NodeKind::UnaryOp { kind: UnaryOpKind::Deref, expr } => {
            let lty = gen_il(*expr, p)?;
            let rty = gen_il(rhs, p)?;
            let lty = match lty {
                Type::Ptr(lty) => lty.borrow().clone(),
                _ => {
                    e0022(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty);
                    return Err(());
                }
            };
            if check_type(&lty, &rty).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty, &rty);
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
            let parent_ty = gen_il(*expr, p)?;
            *p.ret_address.borrow_mut() = false;
            match parent_ty {
                Type::_Self(ref path, ref name, is_mutable) => {
                    let namespace = p.namespace.borrow();
                    let ns = if let Some(ns) = namespace.find(path) {
                        ns
                    } else {
                        let message = format!("failed to resolve: use of undeclared crate or module `{}`", path.join("::"));
                        e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                        return Err(());
                    };
                    if let Some(st) = ns.find_class(|_|true, name) {
                        if let Some(field) = st.borrow().field.find(&ident) {
                            let rty = gen_il(rhs, p)?;
                            if check_type(&field.borrow().ty.borrow(), &rty).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.borrow(), &rty);
                            }
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
                            p.push_il_text(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        } else {
                            e0015(Rc::clone(&p.errors), (p.path, &p.lines, lhs.token), &ident, name);
                        }
                    } else {
                        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), name);
                    }
                }
                Type::Class(_, _, ref path, ref name, _, is_mutable) => {
                    let namespace = p.namespace.borrow();
                    let ns = if let Some(ns) = namespace.find(path) {
                        ns
                    } else {
                        let message = format!("failed to resolve: use of undeclared crate or module `{}`", path.join("::"));
                        e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                        return Err(());
                    };
                    if let Some(cl) = ns.find_class(|_|true, name) {
                        if let Some(field) = cl.borrow().field.find(&ident) {
                            let rty = gen_il(rhs, p)?;
                            if check_type(&field.borrow().ty.borrow(), &rty).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.borrow(), &rty);
                            }
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
                            p.push_il_text(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        } else {
                            e0015(Rc::clone(&p.errors), (p.path, &p.lines, lhs.token), &ident, name);
                        }
                    } else {
                        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), name);
                    }
                }
                ty => {
                    let message = format!("[compiler unimplemented!()] primitive type field: {:?}", ty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                }
            }
        }
        _ => e0019(Rc::clone(&p.errors), (p.path, &p.lines, current_token))
    }
    Ok(Type::Void)
}

fn typing_return<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, expr: Option<Box<Node>>, func_retty: RRType) -> Result<RRType> {
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
        gen_il(*expr, p)?
    } else {
        Type::Void
    };
    if check_type(&rettype, &func_retty.borrow()).is_err() {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &func_retty.borrow(), &rettype);
    }
    p.push_il_text("\tret");
    Ok(rettype)
}

fn typing_break(_current_token: &[Token], st: &mut SymbolTable, p: &Program, brk_label_seq: usize) -> Result<RRType> {
    p.push_il_text(format!("\tbr IL_break{}", brk_label_seq));
    Ok(Type::Void)
}

fn typing_cast<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, new_type: RRType, expr: Node) -> Result<RRType> {
    let old_type = gen_il(expr, p)?;
    match &new_type {
        Type::Numeric(Numeric::I32) => {
            match old_type {
                Type::Numeric(..) | Type::Float(..) | Type::Enum(..) | Type::Bool | Type::Char => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Numeric(Numeric::I32)),
            }
            p.push_il_text("\tconv.i4");
        }
        Type::Float(Float::F32) => {
            match old_type {
                Type::Numeric(..) | Type::Float(Float::F32) => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Float(Float::F32)),
            }
            p.push_il_text("\tconv.r4");
        }
        Type::Bool => {
            match old_type {
                Type::Bool => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Bool),
            }
            p.push_il_text("\tldc.i4.0");
            p.push_il_text("\tcgt");
        }
        Type::Char => {
            match old_type {
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

fn typing_unaryop<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: UnaryOpKind, expr: Node) -> Result<RRType> {
    match kind {
        UnaryOpKind::Not => {
            let ty = gen_il(expr, p)?;
            match ty {
                Type::Bool => {
                    p.push_il_text("\tldc.i4.0");
                    p.push_il_text("\tceq");
                }
                Type::Numeric(_) => {
                    p.push_il_text("\tnot");
                }
                _ => {
                    let message = format!("cannot apply unary operator `!` to type `{}`", ty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                }
            }
            Ok(ty)
        }
        UnaryOpKind::Neg => {
            let ty= gen_il(expr, p)?;
            if let Type::Numeric(..) | Type::Float(..) = ty {
                p.push_il_text("\tneg");
            } else {
                e0021(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ty);
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
                let x = Type::Ptr(RRType::new(obj.ty.borrow().clone()));
                Ok(x)
            } else {
                Ok(Type::Ptr(RRType::new(gen_il(expr, p)?)))
            }
        }
        UnaryOpKind::Deref => {
            let ret_address = *p.ret_address.borrow();
            *p.ret_address.borrow_mut() = false;
            let ty = gen_il(expr, p)?;
            *p.ret_address.borrow_mut() = ret_address;
            match ty {
                Type::Ptr(ty) => {
                    let ty = ty.borrow().clone();
                    match ty {
                        Type::Ptr(_) => p.push_il_text("\tldind.i"),
                        Type::Numeric(Numeric::I32) => p.push_il_text("\tldind.i4"),
                        Type::Float(Float::F32) => p.push_il_text("\tldind.r4"),
                        _ => {
                            let message = format!("[compiler unimplemented!()] dereferenced {:?}", ty);
                            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                        }
                    }
                    Ok(ty)
                }
                Type::Box(ty) => {
                    let ty = ty.borrow().clone();
                    match ty {
                        Type::Class(ClassKind::Struct, ..) => p.push_il_text(format!("\tunbox {}", ty.to_ilstr())),
                        _ => p.push_il_text(format!("\tunbox.any {}", ty.to_ilstr())),
                    }
                    Ok(ty)
                }
                _ =>  {
                    e0022(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ty);
                    Err(())
                }
            }
        }
    }
}

fn typing_binaryop<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: BinaryOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    let ltype = gen_il(lhs, p)?;
    let rtype = gen_il(rhs, p)?;
    let mut is_bool    = false;
    match &ltype {
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
    match (&ltype, &rtype) {
        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => {
            if is_bool {
                Ok(Type::Bool)
            } else {
                Ok(rtype)
            }
        }
        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => {
            if is_bool {
                Ok(Type::Bool)
            } else {
                Ok(ltype)
            }
        }
        _ if ltype == rtype => {
            if is_bool {
                Ok(Type::Bool)
            } else {
                Ok(ltype)
            }
        }
        _ => {
            e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ltype, &rtype);
            Err(())
        }
    }
}

fn typing_shortcircuitop<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    let end_label  = format!("IL_end{}", label_seq());
    match kind {
        ShortCircuitOpKind::And => {
            p.push_il_text("\tldc.i4.0");
            let token = lhs.token;
            let ltype = gen_il(lhs, p)?;
            if ltype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype);
            }
            p.push_il_text(format!("\tbrfalse {}", end_label));
            p.push_il_text("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, p)?;
            if rtype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype);
            }
            p.push_il_text(format!("{}:", end_label));
        }
        ShortCircuitOpKind::Or => {
            p.push_il_text("\tldc.i4.1");
            let token = lhs.token;
            let ltype = gen_il(lhs, p)?;
            if ltype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype);
            }
            p.push_il_text(format!("\tbrtrue {}", end_label));
            p.push_il_text("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, p)?;
            if rtype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype);
            }
            p.push_il_text(format!("{}:", end_label));
        }
    }
    Ok(Type::Bool)
}

fn typing_semi<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, expr: Node) -> Result<RRType> {
    let ty = gen_il(expr, p)?;
    if ty != Type::Void {
        p.push_il_text("\tpop");
    }
    Ok(Type::Void)
}

fn typing_path<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, segment: &str, mut full_path: Vec<String>, child: Node) -> Result<RRType> {
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
            gen_il_path(current_token, p, &segment, full_path, *child)
        }
        NodeKind::Call { name, args } => {
            let namespace = p.namespace.borrow();
            let ns = if let Some(ns) = namespace.find(&full_path) {
                ns
            } else {
                let message = format!("failed to resolve: use of undeclared crate or module `{}`", full_path.join("::"));
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                return Err(());
            };
            if let Some(func) = ns.find_fn(&name) {
                let objs = &func
                    .symbol_table
                    .borrow()
                    .objs;
                let params = objs
                    .iter()
                    .filter(|o| o.borrow().kind == ObjectKind::Param)
                    .collect::<Vec<_>>();
                if params.len() != args.len() {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
                }
                for (arg, param) in args.into_iter().zip(&params) {
                    let token = arg.token;
                    let param = param.borrow();
                    let param_ty = &param.ty.borrow();
                    let arg_ty = gen_il(arg, p)?;
                    if check_type(&arg_ty, param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty);
                    }
                }
                let params = params
                    .iter()
                    .map(|p|p.borrow().ty.borrow().to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                p.push_il_text(format!("\tcall {} '{}'::'{}'({})", func.rettype.borrow().to_ilstr(), p.name, name, params));
                Ok(func.rettype.borrow().clone())
            } else if let Some(im) = ns.find_impl(full_path.last().unwrap()) {
                let func = if let Some(func) = im
                    .functions
                    .iter()
                    .find(|f|f.name==name) {
                        func
                    } else {
                        e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &name, &im.name);
                        return Err(());
                    };
                let objs = &func
                    .symbol_table
                    .borrow()
                    .objs;
                let params = objs
                    .iter()
                    .filter(|o| o.borrow().kind == ObjectKind::Param)
                    .collect::<Vec<_>>();
                if params.len() != args.len() {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
                }
                for (arg, param) in args.into_iter().zip(&params) {
                    let token = arg.token;
                    let param = param.borrow();
                    let param_ty = &param.ty.borrow();
                    let arg_ty = gen_il(arg, p)?;
                    if check_type(&arg_ty, param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty);
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
                Ok(func.rettype.borrow().clone())
            } else {
                e0013(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &name);
                Err(())
            }
        }
        NodeKind::Enum { obj } => {
            let namespace = p.namespace.borrow();
            let ns = if let Some(ns) = namespace.find(&full_path[..full_path.len()-1]) {
                ns
            } else {
                let message = format!("failed to resolve: use of undeclared crate or module `{}`", full_path.join("::"));
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                return Err(());
            };
            if let Some(ed) = ns.find_enum(full_path.last().unwrap()) {
                if let Some(f) = ed.fields.iter().find(|f| f.name == obj.name) {
                    p.push_il_text(format!("\tldc.i4 {}", f.value));
                    Ok(Type::Enum(None, full_path[..full_path.len()-1].to_vec(), full_path.last().unwrap().to_string()))
                } else {
                    e0007(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
                    Err(())
                }
            } else {
                e0007(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
                Err(())
            }
        }
        _ => {
            gen_il(child, p)
        }
    }
}

fn typing_empty() -> Result<RRType> {
    Ok(Type::Void)
}
