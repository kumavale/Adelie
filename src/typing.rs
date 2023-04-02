use crate::ast::*;
use crate::builtin::*;
use crate::class::ClassKind;
use crate::error::*;
use crate::function::Function;
use crate::keyword::{Type, RRType, Numeric, Float, FloatLit};
use crate::namespace::NameSpace;
use crate::object::{FindSymbol, Object, ObjectKind, EnumObject, SymbolTable};
use crate::program::Program;
use crate::token::Token;
use crate::utils::remove_seq;
use std::rc::Rc;
use std::cell::RefCell;

type Result<T> = std::result::Result<T, ()>;

/// 型推論
pub fn type_inference(source: &mut RRType, target: &RRType) {
    match (&mut source.get_type(), &target.get_type()) {
        (_, Type::Unknown)                                   |
        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) |
        (Type::Float(..), Type::Float(Float::F))             => {
            let ty = source.borrow().borrow().clone();
            *source.borrow_mut() = target.borrow().clone();
            *source.borrow().borrow_mut() = ty;
        }
        (Type::Box(l), Type::Box(r)) |
        (Type::Ptr(l), Type::Ptr(r)) => type_inference(l, r),
        _ => (),  // Do nothing
    }
}

/// 型検査
/// アフィン型システム
/// 型推論
pub fn typing<'a>(node: Node, st: &mut SymbolTable, p: &'a Program<'a>, is_ret_address: bool) -> Result<RRType> {
    let ret_address = *p.ret_address.borrow();
    *p.ret_address.borrow_mut() = is_ret_address;
    let ty = match node.kind {
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
        NodeKind::Vec { ty, method } => {
            typing_vec(node.token, st, p, ty, *method)
        }
        NodeKind::Builtin { kind, args } => {
            typing_builtin(node.token, st, kind, args, p)
        }
        NodeKind::Let { obj, init } => {
            typing_let(node.token, st, p, obj, init)
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
            typing_struct(node.token, st, p, obj, field)
        }
        NodeKind::Field { expr, ident } => {
            typing_field(node.token, st, p, *expr, &ident)
        }
        NodeKind::Variable { obj } => {
            typing_variable(node.token, st, p, obj)
        }
        NodeKind::ArrayRef { idx: _, obj } => {
            typing_array_ref(node.token, st, p, obj)
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
        NodeKind::While { cond, then, brk_label_seq: _ } => {
            typing_while(node.token, st, p, *cond, *then)
        }
        NodeKind::Loop { then, brk_label_seq: _ } => {
            typing_loop(node.token, st, p, *then)
        }
        NodeKind::Assign { lhs, rhs } => {
            typing_assign(node.token, st, p, *lhs, *rhs)
        }
        NodeKind::Return { expr, func_retty } => {
            typing_return(node.token, st, p, expr, func_retty)
        }
        NodeKind::Break { brk_label_seq: _ } => {
            typing_break(node.token, st, p)
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
    };
    *p.ret_address.borrow_mut() = ret_address;
    ty
}

fn typing_integer(_current_token: &[Token], st: &mut SymbolTable, p: &Program, ty: RRType, _num: i128) -> Result<RRType> {
    // アドレスが欲しいリテラルは一旦、ローカル変数に格納する
    if *p.ret_address.borrow() {
        let unique_name = format!("integerliteral_{}", crate::seq!());
        let obj = Rc::new(RefCell::new(
                Object::new(unique_name,
                            st.offset(ObjectKind::Local),
                            ObjectKind::Local,
                            RRType::clone(&ty),
                            false)));
        obj.borrow_mut().assigned = true;
        st.push(Rc::clone(&obj));
    }
    Ok(ty)
}

fn typing_float(_current_token: &[Token], st: &mut SymbolTable, p: &Program, ty: RRType, _num: FloatLit) -> Result<RRType> {
    // アドレスが欲しいリテラルは一旦、ローカル変数に格納する
    if *p.ret_address.borrow() {
        let unique_name = format!("floatliteral_{}", crate::seq!());
        let obj = Rc::new(RefCell::new(
                Object::new(unique_name,
                            st.offset(ObjectKind::Local),
                            ObjectKind::Local,
                            RRType::clone(&ty),
                            false)));
        obj.borrow_mut().assigned = true;
        st.push(Rc::clone(&obj));
    }
    Ok(ty)
}

fn typing_string(_current_token: &[Token], _st: &mut SymbolTable, _p: &Program, ty: RRType, _str: &str) -> Result<RRType> {
    // `string`は参照型
    Ok(ty)
}

fn typing_box<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, method: Node) -> Result<RRType> {
    if let NodeKind::Call { name, args } = method.kind {
        match name.as_str() {
            "new" => {
                if args.len() != 1 {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, method.token), 1, args.len());
                }
                let boxed_ty = typing(args.into_iter().next().unwrap(), st, p, false)?;
                Ok(RRType::new(Type::Box(boxed_ty)))
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

fn typing_vec<'a>(_current_token: &[Token], _st: &mut SymbolTable, p: &'a Program<'a>, ty: RRType, method: Node) -> Result<RRType> {
    if let NodeKind::Call { name, args } = method.kind {
        match name.as_str() {
            "new" => {
                if !args.is_empty() {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, method.token), 0, args.len());
                }
                Ok(RRType::clone(&ty))
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

fn typing_let<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, obj: Rc<RefCell<Object>>, init: Option<Box<Node>>) -> Result<RRType> {
    if let Some(init) = init {
        if let Some(parent) = &obj.borrow().parent {
            // let文の左辺が{ident}.{field}の形になるのは、クロージャの自由変数を参照している場合のみ
            let ident = obj.borrow().name.to_string();
            *p.consume.borrow_mut() = false;
            let parent_ty = typing_variable(current_token, st, p, parent.clone())?;
            let parent_ty = parent_ty.get_type();
            *p.consume.borrow_mut() = true;
            match &parent_ty {
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
                        if cl.borrow().field.find(&ident).is_some() {
                            let rty = typing(*init, st, p, false)?;
                            let mut obj_ty = obj.borrow().ty.clone();
                            type_inference(&mut obj_ty, &rty);
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
                        } else {
                            e0015(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ident, name);
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
            // 変数をシンボルテーブルに格納する必要はないので早期リターン
            return Ok(RRType::new(Type::Void));
        }
        let mut rty = typing(*init, st, p, false)?;
        if obj.borrow().ty.get_type() == Type::Unknown {
            type_inference(&mut rty, &obj.borrow_mut().ty);
            //dbg!(&obj.borrow().name, obj.borrow().ty.get_type());
            debug_assert_ne!(obj.borrow().ty.get_type(), Type::Unknown);
        } else {
            //dbg!(&obj.borrow().name, obj.borrow().ty.get_type());
            type_inference(&mut obj.borrow_mut().ty, &rty);
        }
        let is_assigned = obj.borrow().is_assigned();
        obj.borrow_mut().assigned = true;
        let obj = obj.borrow();
        if !obj.is_mutable() && is_assigned {
            e0028(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
        }
    }
    st.push(obj);
    Ok(RRType::new(Type::Void))
}

fn typing_call<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, name: &str, args: Vec<Node>) -> Result<RRType> {
    if let Some(func) = p.namespace.borrow().find_fn(name) {
        let objs = if let Ok(st) = func.symbol_table.try_borrow() {
            st.objs.clone()
        } else {
            st.objs.clone()
        };
        let params = objs
            .iter()
            .filter(|o| o.borrow().kind == ObjectKind::Param)
            .collect::<Vec<_>>();
        if params.len() != args.len() {
            e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
        }
        for (arg, param) in args.into_iter().zip(&params) {
            let arg_ty = typing(arg, st, p, false)?;
            type_inference(&mut param.borrow_mut().ty, &arg_ty);
            debug_assert_ne!(&arg_ty.get_type(), &Type::Numeric(Numeric::Integer));
        }
        Ok(func.rettype.clone())
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
    *p.consume.borrow_mut() = false;
    let parent_ty = typing(expr, st, p, true)?;
    let parent_ty = parent_ty.get_type();
    *p.consume.borrow_mut() = true;
    match &parent_ty {
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
                        if let Type::Class(.., name, base, _) = &base.get_type() {
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
                    let arg_ty = typing(arg, st, p, false)?;
                    type_inference(&mut param.borrow_mut().ty, &arg_ty);
                    debug_assert_ne!(&arg_ty.get_type(), &Type::Numeric(Numeric::Integer));
                }
                Ok(func.rettype.clone())
            } else {
                e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, cl_name);
                Err(())
            }
        }
        // 仮実装
        Type::Numeric(Numeric::I32) if ident == "to_string" => {
            Ok(RRType::new(Type::String))
        }
        // 仮実装
        Type::Vec(ty) if ident == "push" => {
            let arg_ty = typing(args.into_iter().next().unwrap(), st, p, false)?;
            type_inference(&mut RRType::clone(ty), &arg_ty);
            Ok(RRType::new(Type::Void))
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
    _st: &mut SymbolTable,
    _p: &'a Program<'a>,
    ty: RRType,
    _ident: &str,
    _parent: &str,
) -> Result<RRType> {
    Ok(ty)
}

fn typing_struct<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, obj: Rc<RefCell<Object>>, field: Vec<Node>) -> Result<RRType> {
    let ns = p.namespace.borrow();
    let (ns, name) = if let Type::Class(ClassKind::Struct, _, path, name, ..) = &obj.borrow().ty.get_type() {
        if let Some(ns) = ns.find(path) {
            (ns, name.to_string())
        } else {
            let message = format!("failed to resolve: use of undeclared type `{}`", path.join("::"));
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            return Err(());
        }
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &remove_seq(&obj.borrow().name));
        return Err(());
    };
    if let Some(cl) = ns.find_class(|k|k==&ClassKind::Struct, &name) {
        if field.len() != cl.borrow().field.objs.len() {
            e0017(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &cl.borrow().name);
        }
        for (field_expr, field_dec) in field.into_iter().zip(&cl.borrow().field.objs) {
            let ty = typing(field_expr, st, p, false)?;
            type_inference(&mut field_dec.borrow_mut().ty, &ty);
            debug_assert_ne!(&ty.get_type(), &Type::Numeric(Numeric::Integer));
        }
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &remove_seq(&obj.borrow().name));
    }
    st.push(obj.clone());
    Ok(obj.borrow().ty.clone())
}

fn typing_field<'a>(
    current_token: &[Token],
    st: &mut SymbolTable,
    p: &'a Program<'a>,
    expr: Node,
    ident: &str,
) -> Result<RRType> {
    *p.consume.borrow_mut() = false;
    let parent_ty = typing(expr, st, p, true)?;
    let parent_ty = parent_ty.get_type();
    *p.consume.borrow_mut() = true;
    let (path, parent_name, base, is_mutable) = match &parent_ty {
        Type::_Self(path, name, is_mutable) => {
            (path.to_vec(), name.to_string(), &None, *is_mutable)
        }
        Type::Class(_, _, path, name, base, is_mutable) => {
            (path.to_vec(), name.to_string(), base, *is_mutable)
        }
        Type::Ptr(ty) => {
            match &ty.get_type() {
                Type::_Self(path, name, is_mutable) => {
                    (path.to_vec(), name.to_string(), &None, *is_mutable)
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
            if is_mutable {
                Ok(field.borrow().ty.clone().into_mutable())
            } else {
                Ok(field.borrow().ty.clone())
            }
        } else {
            // baseクラスから検索
            fn search_from_base(base_ty: &Option<RRType>, p: &Program, ident: &str, is_mutable: bool) -> Result<RRType> {
                if let Some(base_ty) = base_ty {
                    let ns = p.namespace.borrow();
                    let base_ty = base_ty.get_type();
                    let (path, name, base)  = if let Type::Class(_, _, p, n, b, ..) = &base_ty { (p, n, b) } else { unreachable!() };
                    if let Some(ns) = ns.find(path) {
                        if let Some(cl) = ns.find_class(|_|true, name) {
                            if let Some(field) = cl.borrow().field.find(ident) {
                                if is_mutable {
                                    return Ok(field.borrow().ty.clone().into_mutable());
                                } else {
                                    return Ok(field.borrow().ty.clone());
                                }
                            }
                        }
                    }
                    return search_from_base(base, p, ident, is_mutable);
                }
                Err(())
            }
            search_from_base(base, p, ident, is_mutable)
                .map_err(|()| e0015(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, &parent_name))
        }
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &parent_name);
        Err(())
    }
}

fn typing_variable(current_token: &[Token], st: &mut SymbolTable, p: &Program, obj: Rc<RefCell<Object>>) -> Result<RRType> {
    if let Some(parent) = &obj.borrow().parent {
        if !obj.borrow().assigned {
            //e0027(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().name);
            //return Err(());
            // TODO: objのis_assignedを再帰的にtrueにする必要がある
            //dbg!(&obj);
            let message = format!("[compiler unimplemented!()] use of possibly-uninitialized variable: `{}`", obj.borrow().name);
            warning(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
        }
        let parent_ty = typing_variable(current_token, st, p, parent.clone())?;
        let parent_ty = parent_ty.get_type();
        let ident = obj.borrow().name.to_string();
        if let Type::Class(_, _, ref path, ref parent_name, _base, _is_mutable) = &parent_ty {
            let ns = p.namespace.borrow();
            let ns = ns.find(path).unwrap();
            if let Some(cl) = ns.find_class(|_|true, parent_name) {
                if cl.borrow().field.find(&ident).is_some() {
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
        if st.find_mut(&obj.borrow().name).is_none() {
            if obj.borrow().name.ends_with(">nested_class") {
                let message = format!("[compiler unimplemented!()] cannot find value: `{}`", &obj.borrow().name);
                warning(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            } else {
                e0007(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().name);
                return Err(());
            }
        }
        if !obj.borrow().ty.get_type().copyable() && obj.borrow().used {
            let message = format!("use of moved value: `{}`", obj.borrow().name);
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            return Err(());
        }
        if !obj.borrow().assigned {
            e0027(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().name);
            return Err(());
        }
    }

    // move
    p.consume.borrow().then(|| obj.borrow_mut().consume());

    if obj.borrow().is_mutable() {
        Ok(obj.borrow().ty.clone().into_mutable())
    } else {
        Ok(obj.borrow().ty.clone())
    }
}

fn typing_array_ref(_current_token: &[Token], _st: &mut SymbolTable, _p: &Program, obj: Rc<RefCell<Object>>) -> Result<RRType> {
    // TODO: 変数チェック
    //if st.find_mut(&obj.borrow().name).is_none() {
    //    if obj.borrow().name.ends_with(">nested_class") {
    //        let message = format!("[compiler unimplemented!()] cannot find value: `{}`", &obj.borrow().name);
    //        warning(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
    //    } else {
    //        e0007(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().name);
    //        return Err(());
    //    }
    //}
    //if !obj.borrow().ty.get_type().copyable() && obj.borrow().used {
    //    let message = format!("use of moved value: `{}`", obj.borrow().name);
    //    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
    //    return Err(());
    //}
    //if !obj.borrow().assigned {
    //    e0027(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.borrow().name);
    //    return Err(());
    //}

    // TODO: 型チェック

    // move
    //p.consume.borrow().then(|| obj.borrow_mut().consume());

    match obj.borrow().ty.get_type() {
        Type::Vec(ty) => {
            Ok(ty)
        }
        _ => unimplemented!()
    }
}

fn typing_enum(current_token: &[Token], _st: &mut SymbolTable, p: &Program, obj: EnumObject) -> Result<RRType> {
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
                ty = typing(stmt, st, p, false)?;
                break;
            }
            _ => {
                ty = typing(stmt, st, p, false)?;
            }
        }
    }
    st.leave_scope();
    Ok(ty)
}

fn typing_if<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, cond: Node, then: Node, els: Option<Box<Node>>) -> Result<RRType> {
    let token = cond.token;
    let cond_type = typing(cond, st, p, false)?;
    if cond_type.get_type() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.get_type());
    }
    let mut then_type = typing(then, st, p, false)?;
    let els = els.map(|els| (els.token, typing(*els, st, p, false)));
    if let Some(els) = els {
        let mut els_type = els.1?;
        type_inference(&mut els_type, &then_type);
        type_inference(&mut then_type, &els_type);
    }
    Ok(then_type)
}

fn typing_while<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, cond: Node, then: Node) -> Result<RRType> {
    let token = cond.token;
    let cond_type = typing(cond, st, p, false)?;
    if cond_type.get_type() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.get_type());
    }
    let _then_token = then.token;
    let _then_type = typing(then, st, p, false)?;
    //if !matches!(*then_type.borrow(), /* TODO: Type::Never(_) | */ Type::Void) {
    //    e0012(Rc::clone(&p.errors), (p.path, &p.lines, then_token), &Type::Void, &then_type.borrow());
    //}
    Ok(RRType::new(Type::Void))
}

fn typing_loop<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, then: Node) -> Result<RRType> {
    let _then_token = then.token;
    let _then_type = typing(then, st, p, false)?;
    //if !matches!(*then_type.borrow(), /* TODO: Type::Never(_) | */ Type::Void) {
    //    e0012(Rc::clone(&p.errors), (p.path, &p.lines, then_token), &Type::Void, &then_type.borrow());
    //}
    Ok(RRType::new(Type::Void))
}

fn typing_assign<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, lhs: Node, rhs: Node) -> Result<RRType> {
    match lhs.kind {
        NodeKind::Variable { obj } => {
            if let Some(parent) = &obj.borrow().parent {
                let ident = obj.borrow().name.to_string();
                let parent_ty = typing_variable(current_token, st, p, parent.clone())?;
                let parent_ty = parent_ty.get_type();
                match &parent_ty {
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
                                let rty = typing(rhs, st, p, false)?;
                                type_inference(&mut field.borrow_mut().ty, &rty);
                                if !is_mutable {
                                    let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                                }
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
                return Ok(RRType::new(Type::Void));
            }
            let mut rty = typing(rhs, st, p, false)?;
            type_inference(&mut obj.borrow_mut().ty, &rty);
            type_inference(&mut rty, &obj.borrow().ty);
            let is_assigned = obj.borrow().is_assigned();
            obj.borrow_mut().assigned = true;

            // 所有権の復活
            obj.borrow_mut().used = false;

            let obj = obj.borrow();
            if !obj.is_mutable() && is_assigned {
                e0028(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
            }
        }
        NodeKind::UnaryOp { kind: UnaryOpKind::Deref, expr } => {
            // TODO: この処理じゃ`**a=b`とか出来ないのでは？
            let lty = typing(*expr, st, p, false)?;
            let rty = typing(rhs, st, p, false)?;
            let mut lty = match lty.get_type() {
                Type::Ptr(lty) => lty,
                _ => {
                    e0022(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty.get_type());
                    return Err(());
                }
            };
            type_inference(&mut lty, &rty);
            // TODO: この処理じゃ`char`型変数のDerefとか出来ないのでは？
            match lty.get_type() {
                Type::Ptr(_) | Type::Numeric(Numeric::I32) => (),
                _ => {
                    let message = format!("[compiler unimplemented!()] dereference {:?}", lty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                }
            }
        }
        NodeKind::Field { expr, ident } => {
            *p.consume.borrow_mut() = false;
            let parent_ty = typing(*expr, st, p, true)?;
            let parent_ty = parent_ty.get_type();
            *p.consume.borrow_mut() = true;
            match &parent_ty {
                Type::_Self(ref path, ref name, is_mutable) => {
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
                            let rty = typing(rhs, st, p, false)?;
                            type_inference(&mut field.borrow_mut().ty, &rty);
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
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
                            let rty = typing(rhs, st, p, false)?;
                            type_inference(&mut field.borrow_mut().ty, &rty);
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
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
        kind => {
            dbg!(&kind);
            e0019(Rc::clone(&p.errors), (p.path, &p.lines, current_token));
        }
    }
    Ok(RRType::new(Type::Void))
}

fn typing_return<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, expr: Option<Box<Node>>, mut func_retty: RRType) -> Result<RRType> {
    let rettype = if let Some(expr) = expr {
        typing(*expr, st, p, false)?
    } else {
        RRType::new(Type::Void)
    };

    type_inference(&mut func_retty, &rettype);
    debug_assert_ne!(&rettype.get_type(), &Type::Numeric(Numeric::Integer));

    // TODO: Type::Never(rettype)
    Ok(rettype)
}

fn typing_break(_current_token: &[Token], _st: &mut SymbolTable, _p: &Program) -> Result<RRType> {
    // TODO: Type::Never(rettype)
    Ok(RRType::new(Type::Void))
}

fn typing_cast<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, new_type: RRType, expr: Node) -> Result<RRType> {
    let _old_type = typing(expr, st, p, false)?;
    Ok(new_type)
}

fn typing_unaryop<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: UnaryOpKind, expr: Node) -> Result<RRType> {
    match kind {
        UnaryOpKind::Not => {
            Ok(typing(expr, st, p, false)?)
        }
        UnaryOpKind::Neg => {
            Ok(typing(expr, st, p, false)?)
        }
        UnaryOpKind::Ref => {
            if let NodeKind::Variable { obj } = expr.kind {
                Ok(RRType::new(Type::Ptr(obj.borrow().ty.clone())))
            } else {
                Ok(RRType::new(Type::Ptr(typing(expr, st, p, false)?)))
            }
        }
        UnaryOpKind::Deref => {
            *p.consume.borrow_mut() = false;
            let ty = typing(expr, st, p, false)?;
            let ty = ty.get_type();
            *p.consume.borrow_mut() = true;
            match &ty {
                Type::Ptr(ty) => {
                    match &ty.get_type() {
                        Type::Ptr(_) | Type::Numeric(..) | Type::Float(..) => (),
                        ty => {
                            let message = format!("[compiler unimplemented!()] dereferenced {:?}", ty);
                            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                        }
                    }
                    Ok(ty.clone())
                }
                Type::Box(ty) => {
                    Ok(ty.clone())
                }
                ty =>  {
                    e0022(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ty);
                    Err(())
                }
            }
        }
    }
}

fn typing_binaryop<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: BinaryOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    let mut ltype = typing(lhs, st, p, false)?;
    let mut rtype = typing(rhs, st, p, false)?;
    let is_bool = matches!(kind,
        BinaryOpKind::Eq |
        BinaryOpKind::Lt |
        BinaryOpKind::Le |
        BinaryOpKind::Ne |
        BinaryOpKind::Gt |
        BinaryOpKind::Ge
    );
    type_inference(&mut ltype, &rtype);
    type_inference(&mut rtype, &ltype);
    if is_bool {
        Ok(RRType::new(Type::Bool))
    } else {
        Ok(ltype)
    }
}

fn typing_shortcircuitop<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    match kind {
        ShortCircuitOpKind::And |
        ShortCircuitOpKind::Or  => {
            let ltype = typing(lhs, st, p, false)?;
            type_inference(&mut RRType::new(Type::Bool), &ltype);
            let rtype = typing(rhs, st, p, false)?;
            type_inference(&mut RRType::new(Type::Bool), &rtype);
        }
    }
    Ok(RRType::new(Type::Bool))
}

fn typing_semi<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, expr: Node) -> Result<RRType> {
    typing(expr, st, p, false)?;
    Ok(RRType::new(Type::Void))
}

fn typing_path<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, _segment: &str, mut full_path: Vec<String>, child: Node) -> Result<RRType> {
    match child.kind {
        NodeKind::Path { segment, child } => {
            full_path.push(segment.to_string());
            typing_path(current_token, st, p, &segment, full_path, *child)
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
                    let arg_ty = typing(arg, st, p, false)?;
                    type_inference(&mut param.borrow_mut().ty, &arg_ty);
                    debug_assert_ne!(&arg_ty.get_type(), &Type::Numeric(Numeric::Integer));
                }
                Ok(func.rettype.clone())
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
                    let arg_ty = typing(arg, st, p, false)?;
                    type_inference(&mut param.borrow_mut().ty, &arg_ty);
                    debug_assert_ne!(&arg_ty.get_type(), &Type::Numeric(Numeric::Integer));
                }
                Ok(func.rettype.clone())
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
                if ed.fields.iter().any(|f| f.name == obj.name) {
                    Ok(RRType::new(Type::Enum(None, full_path[..full_path.len()-1].to_vec(), full_path.last().unwrap().to_string())))
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
            typing(child, st, p, false)
        }
    }
}

fn typing_empty() -> Result<RRType> {
    Ok(RRType::new(Type::Void))
}
