use crate::ast::*;
use crate::builtin::*;
use crate::class::ClassKind;
use crate::error::*;
use crate::function::Function;
use crate::keyword::{Type, RRType, Numeric, Float, FloatNum};
use crate::namespace::NameSpace;
use crate::object::{FindSymbol, Object, ObjectKind, EnumObject, SymbolTable};
use crate::program::Program;
use crate::token::Token;
use crate::utils::remove_seq;
use std::rc::Rc;
use std::cell::RefCell;

type Result<T> = std::result::Result<T, ()>;

/// 型推論
fn type_inference(source: &RRType, target: &mut RRType) {
    match (&source.get_type(), &mut target.get_type()) {
        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => (),
        (Type::Box(l), Type::Box(r)) |
        (Type::Ptr(l), Type::Ptr(r)) => type_inference(l, r),
        _ => {
            /* TODO */
            return;
        }
    }
    *target.borrow_mut() = source.borrow().clone();
}

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
    }
}

fn typing_integer(_current_token: &[Token], _st: &mut SymbolTable, _p: &Program, ty: RRType, _num: i128) -> Result<RRType> {
    // 型推論の流れ
    // let x = 42;      // 42: Integer, x: Integer
    // let y: i32 = x;  //  x: Integer, y: I32
    // `x`が`y`に束縛される段階で、`x`の型が決定される。
    // `42`の`RRType`を`x`にRcして、`x`を`y`に束縛するときに`x`の`RRType`を`i32`にすれば、
    // `42`,`x`,`y`のすべてが`i32`になる。
    Ok(ty)
}

fn typing_float(_current_token: &[Token], _st: &mut SymbolTable, _p: &Program, ty: RRType, _num: FloatNum) -> Result<RRType> {
    Ok(ty)
}

fn typing_string(_current_token: &[Token], _st: &mut SymbolTable, _p: &Program, ty: RRType, _str: &str) -> Result<RRType> {
    Ok(ty)
}

fn typing_box<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, method: Node) -> Result<RRType> {
    if let NodeKind::Call { name, args } = method.kind {
        match name.as_str() {
            "new" => {
                if args.len() != 1 {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, method.token), 1, args.len());
                }
                let boxed_ty = typing(args.into_iter().next().unwrap(), st, p)?;
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

fn typing_let<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, obj: Rc<RefCell<Object>>, init: Option<Box<Node>>) -> Result<RRType> {
    if let Some(init) = init {
        if let Some(parent) = &obj.borrow().parent {
            // let文の左辺が{ident}.{field}の形になるのは、クロージャの自由変数を参照している場合のみ
            let ident = obj.borrow().name.to_string();
            *p.ret_address.borrow_mut() = true;
            *p.consume.borrow_mut() = false;
            let parent_ty = typing_variable(current_token, st, p, parent.clone())?;
            let parent_ty = parent_ty.get_type();
            *p.consume.borrow_mut() = true;
            *p.ret_address.borrow_mut() = false;
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
                            let mut rty = typing(*init, st, p)?;
                            type_inference(&obj.borrow().ty, &mut rty);
                            debug_assert_ne!(&rty.get_type(), &Type::Numeric(Numeric::Integer));
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
        let mut rty = typing(*init, st, p)?;
        type_inference(&obj.borrow().ty, &mut rty);
        debug_assert_ne!(&rty.get_type(), &Type::Numeric(Numeric::Integer));
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
            let mut arg_ty = typing(arg, st, p)?;
            type_inference(&param.borrow().ty, &mut arg_ty);
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
    fn check_type(arg_ty: &Type, param_ty: &Type) -> Result<()> {
        match (arg_ty, param_ty) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.get_type(), &r.get_type()),
            _ if arg_ty == param_ty => Ok(()),
            (Type::Class(.., base_ty, _), Type::Class(..)) => {
                fn check_base(base_ty: &Option<RRType>, param_ty: &Type) -> std::result::Result<(), ()> {
                    if let Some(base_ty) = base_ty {
                        let base_ty = base_ty.get_type();
                        if base_ty != *param_ty {
                            let base_ty = if let Type::Class(.., b, _) = &base_ty { b } else { unreachable!() };
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
    let parent_ty = typing(expr, st, p)?;
    let parent_ty = parent_ty.get_type();
    *p.consume.borrow_mut() = true;
    *p.ret_address.borrow_mut() = false;
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
                    let token = arg.token;
                    let arg_ty = typing(arg, st, p)?;
                    let param = param.borrow();
                    let param_ty = &param.ty.get_type();
                    if check_type(&arg_ty.get_type(), param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty.get_type());
                    }
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
    fn check_type(then: &Type, els: &Type) -> Result<RRType> {
        match (then, els) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(RRType::new(els.clone())),
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(RRType::new(then.clone())),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.get_type(), &r.get_type()),
            _ if then == els => Ok(RRType::new(then.clone())),
            _ => Err(())
        }
    }
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
            let field_token = field_expr.token;
            let ty = typing(field_expr, st, p)?;
            if check_type(&ty.get_type(), &field_dec.borrow().ty.get_type()).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, field_token), &field_dec.borrow().ty.get_type(), &ty.get_type());
            }
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
    let parent_ty = typing(expr, st, p)?;
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

fn typing_if<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, cond: Node, then: Node, els: Option<Box<Node>>) -> Result<RRType> {
    let token = cond.token;
    let cond_type = typing(cond, st, p)?;
    if cond_type.get_type() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.get_type());
    }
    let mut then_type = typing(then, st, p)?;
    let els = els.map(|els| (els.token, typing(*els, st, p)));
    if let Some(els) = els {
        let mut els_type = els.1?;
        type_inference(&els_type, &mut then_type);
        type_inference(&then_type, &mut els_type);
        // then: Integer, else: Integerの場合、両方に同じRRTypeをcloneする
        // TODO: この処理を`type_inference()`に合成
        match (&then_type.get_type(), &els_type.get_type()) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(Numeric::Integer)) => {
                *els_type.borrow_mut() = then_type.borrow().clone();
            }
            _ => ()
        }
    }
    Ok(then_type)
}

fn typing_while<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, cond: Node, then: Node) -> Result<RRType> {
    let token = cond.token;
    let cond_type = typing(cond, st, p)?;
    if cond_type.get_type() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type.get_type());
    }
    let _then_token = then.token;
    let _then_type = typing(then, st, p)?;
    //if !matches!(*then_type.borrow(), /* TODO: Type::Never(_) | */ Type::Void) {
    //    e0012(Rc::clone(&p.errors), (p.path, &p.lines, then_token), &Type::Void, &then_type.borrow());
    //}
    Ok(RRType::new(Type::Void))
}

fn typing_loop<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, then: Node) -> Result<RRType> {
    let _then_token = then.token;
    let _then_type = typing(then, st, p)?;
    //if !matches!(*then_type.borrow(), /* TODO: Type::Never(_) | */ Type::Void) {
    //    e0012(Rc::clone(&p.errors), (p.path, &p.lines, then_token), &Type::Void, &then_type.borrow());
    //}
    Ok(RRType::new(Type::Void))
}

fn typing_assign<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, lhs: Node, rhs: Node) -> Result<RRType> {
    fn check_type(lty: &Type, rty: &Type) -> Result<()> {
        match (lty, rty) {
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.get_type(), &r.get_type()),
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
                let parent_ty = parent_ty.get_type();
                *p.ret_address.borrow_mut() = false;
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
                                let rty = typing(rhs, st, p)?;
                                if check_type(&field.borrow().ty.get_type(), &rty.get_type()).is_err() {
                                    e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.get_type(), &rty.get_type());
                                }
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
            let rty = typing(rhs, st, p)?;
            let is_assigned = obj.borrow().is_assigned();
            obj.borrow_mut().assigned = true;

            // 所有権の復活
            obj.borrow_mut().used = false;

            let obj = obj.borrow();
            if !obj.is_mutable() && is_assigned {
                e0028(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
            }
            if check_type(&obj.ty.get_type(), &rty.get_type()).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.ty.get_type(), &rty.get_type());
            }
        }
        NodeKind::UnaryOp { kind: UnaryOpKind::Deref, expr } => {
            let lty = typing(*expr, st, p)?;
            let rty = typing(rhs, st, p)?;
            let lty = match &lty.get_type() {
                Type::Ptr(lty) => lty.get_type(),
                _ => {
                    e0022(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty.get_type());
                    return Err(());
                }
            };
            if check_type(&lty, &rty.get_type()).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty, &rty.get_type());
            }
            match lty {
                Type::Ptr(_) | Type::Numeric(Numeric::I32) => (),
                _ => {
                    let message = format!("[compiler unimplemented!()] dereference {:?}", lty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                }
            }
        }
        NodeKind::Field { expr, ident } => {
            *p.ret_address.borrow_mut() = true;
            *p.consume.borrow_mut() = false;
            let parent_ty = typing(*expr, st, p)?;
            let parent_ty = parent_ty.get_type();
            *p.consume.borrow_mut() = true;
            *p.ret_address.borrow_mut() = false;
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
                            let rty = typing(rhs, st, p)?;
                            if check_type(&field.borrow().ty.get_type(), &rty.get_type()).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.get_type(), &rty.get_type());
                            }
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
                            let rty = typing(rhs, st, p)?;
                            if check_type(&field.borrow().ty.get_type(), &rty.get_type()).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.get_type(), &rty.get_type());
                            }
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

fn typing_return<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, expr: Option<Box<Node>>, func_retty: RRType) -> Result<RRType> {
    fn check_type(arg: &Type, param: &Type) -> Result<()> {
        match (arg, param) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.get_type(), &r.get_type()),
            _ if arg == param => Ok(()),
            _ => Err(())
        }
    }
    let mut rettype = if let Some(expr) = expr {
        typing(*expr, st, p)?
    } else {
        RRType::new(Type::Void)
    };

    type_inference(&func_retty, &mut rettype);
    debug_assert_ne!(&rettype.get_type(), &Type::Numeric(Numeric::Integer));

    if check_type(&rettype.get_type(), &func_retty.get_type()).is_err() {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &func_retty.get_type(), &rettype.get_type());
    }
    // TODO: Type::Never(rettype)
    Ok(rettype)
}

fn typing_break(_current_token: &[Token], _st: &mut SymbolTable, _p: &Program) -> Result<RRType> {
    // TODO: Type::Never(rettype)
    Ok(RRType::new(Type::Void))
}

fn typing_cast<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, new_type: RRType, expr: Node) -> Result<RRType> {
    let old_type = typing(expr, st, p)?;
    let old_type = old_type.get_type();
    match &new_type.get_type() {
        Type::Numeric(Numeric::I32) => {
            match old_type {
                Type::Numeric(..) | Type::Float(..) | Type::Enum(..) | Type::Bool | Type::Char => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Numeric(Numeric::I32)),
            }
        }
        Type::Float(Float::F32) => {
            match old_type {
                Type::Numeric(..) | Type::Float(Float::F32) => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Float(Float::F32)),
            }
        }
        Type::Bool => {
            match old_type {
                Type::Bool => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Bool),
            }
        }
        Type::Char => {
            match old_type {
                Type::Char | Type::Numeric(_) => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Char),
            }
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
            let ty = typing(expr, st, p)?;
            match &ty.get_type() {
                Type::Bool | Type::Numeric(_) => (),
                ty => {
                    let message = format!("cannot apply unary operator `!` to type `{}`", ty);
                    e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                }
            }
            Ok(ty)
        }
        UnaryOpKind::Neg => {
            let ty= typing(expr, st, p)?;
            match &ty.get_type() {
                Type::Numeric(..) | Type::Float(..) => (),
                ty => {
                    e0021(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ty);
                }
            }
            Ok(ty)
        }
        UnaryOpKind::Ref => {
            if let NodeKind::Variable { obj } = expr.kind {
                Ok(RRType::new(Type::Ptr(obj.borrow().ty.clone())))
            } else {
                Ok(RRType::new(Type::Ptr(typing(expr, st, p)?)))
            }
        }
        UnaryOpKind::Deref => {
            let ret_address = *p.ret_address.borrow();
            *p.ret_address.borrow_mut() = false;
            *p.consume.borrow_mut() = false;
            let ty = typing(expr, st, p)?;
            let ty = ty.get_type();
            *p.consume.borrow_mut() = true;
            *p.ret_address.borrow_mut() = ret_address;
            match &ty {
                Type::Ptr(ty) => {
                    match &ty.get_type() {
                        Type::Ptr(_) | Type::Numeric(Numeric::I32) | Type::Float(Float::F32) => (),
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

fn typing_binaryop<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: BinaryOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    let ltype = typing(lhs, st, p)?;
    let ltype = ltype.get_type();
    let rtype = typing(rhs, st, p)?;
    let rtype = rtype.get_type();
    let mut is_bool = false;
    match &ltype {
        Type::Numeric(..) => match kind {
            BinaryOpKind::Add    |
            BinaryOpKind::Sub    |
            BinaryOpKind::Mul    |
            BinaryOpKind::Div    |
            BinaryOpKind::Rem    |
            BinaryOpKind::BitXor |
            BinaryOpKind::BitAnd |
            BinaryOpKind::BitOr  |
            BinaryOpKind::Shl    |
            BinaryOpKind::Shr    => {
                // Do nothing
            }
            BinaryOpKind::Eq |
            BinaryOpKind::Lt |
            BinaryOpKind::Le |
            BinaryOpKind::Ne |
            BinaryOpKind::Gt |
            BinaryOpKind::Ge => {
                is_bool = true;
            }
        }
        Type::Float(..) => match kind {
            BinaryOpKind::Add |
            BinaryOpKind::Sub |
            BinaryOpKind::Mul |
            BinaryOpKind::Div |
            BinaryOpKind::Rem => {
                // Do nothing
            }
            BinaryOpKind::Eq |
            BinaryOpKind::Lt |
            BinaryOpKind::Le |
            BinaryOpKind::Ne |
            BinaryOpKind::Gt |
            BinaryOpKind::Ge => {
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
            BinaryOpKind::Eq |
            BinaryOpKind::Lt |
            BinaryOpKind::Le |
            BinaryOpKind::Ne |
            BinaryOpKind::Gt |
            BinaryOpKind::Ge => {
                is_bool = true;
            }
            _ => {
                e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
        }
        Type::String => match kind {
            BinaryOpKind::Add => {
                // Do nothing
            }
            BinaryOpKind::Sub |
            BinaryOpKind::Mul |
            BinaryOpKind::Div |
            BinaryOpKind::Rem => {
                e0023(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
            BinaryOpKind::Eq |
            BinaryOpKind::Lt |
            BinaryOpKind::Le |
            BinaryOpKind::Ne |
            BinaryOpKind::Gt |
            BinaryOpKind::Ge => {
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
        _ if ltype == rtype => {
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

fn typing_shortcircuitop<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Result<RRType> {
    match kind {
        ShortCircuitOpKind::And |
        ShortCircuitOpKind::Or  => {
            let token = lhs.token;
            let ltype = typing(lhs, st, p)?;
            if ltype.get_type() != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype.get_type());
            }
            let token = rhs.token;
            let rtype = typing(rhs, st, p)?;
            if rtype.get_type() != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype.get_type());
            }
        }
    }
    Ok(RRType::new(Type::Bool))
}

fn typing_semi<'a>(_current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, expr: Node) -> Result<RRType> {
    typing(expr, st, p)?;
    Ok(RRType::new(Type::Void))
}

fn typing_path<'a>(current_token: &[Token], st: &mut SymbolTable, p: &'a Program<'a>, _segment: &str, mut full_path: Vec<String>, child: Node) -> Result<RRType> {
    fn check_type(arg: &Type, param: &Type) -> Result<()> {
        match (arg, param) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.get_type(), &r.get_type()),
            _ if arg == param => Ok(()),
            _ => Err(())
        }
    }
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
                    let token = arg.token;
                    let param = param.borrow();
                    let param_ty = &param.ty.get_type();
                    let arg_ty = typing(arg, st, p)?;
                    if check_type(&arg_ty.get_type(), param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty.get_type());
                    }
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
                    let token = arg.token;
                    let param = param.borrow();
                    let param_ty = &param.ty.get_type();
                    let arg_ty = typing(arg, st, p)?;
                    if check_type(&arg_ty.get_type(), param_ty).is_err() {
                        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty.get_type());
                    }
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
            typing(child, st, p)
        }
    }
}

fn typing_empty() -> Result<RRType> {
    Ok(RRType::new(Type::Void))
}

fn typing_builtin<'a>(token: &[Token], st: &mut SymbolTable, kind: Builtin, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    match kind {
        Builtin::Assert   => typing_builtin_assert(token, st, args, p),
        Builtin::AssertEq => typing_builtin_assert_eq(token, st, args, p),
        Builtin::Panic    => typing_builtin_panic(token, st, args, p),
        Builtin::Print    => typing_builtin_print(token, st, args, p),
        Builtin::Println  => typing_builtin_println(token, st, args, p),
        Builtin::ReadLine => typing_builtin_read_line(token, st, args, p),
    }
}

fn typing_builtin_assert<'a>(token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    if args.len() != 1 {
        e0029(Rc::clone(&p.errors), (p.path, &p.lines, token), 1, args.len());
        return Err(());
    }
    let arg = args.pop().unwrap();
    let ty = typing(arg, st, p)?;
    if ty.get_type() != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ty.get_type());
    }
    Ok(RRType::new(Type::Void))
}

fn typing_builtin_assert_eq<'a>(token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    fn check_type(lty: &Type, rty: &Type) -> std::result::Result<(), ()> {
        match (&lty, &rty) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(()),
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.get_type(), &r.get_type()),
            _ if lty == rty => Ok(()),
            _ => Err(())
        }
    }
    if args.len() != 2 {
        e0029(Rc::clone(&p.errors), (p.path, &p.lines, token), 2, args.len());
        return Err(());
    }
    let rhs = args.pop().unwrap();
    let lhs = args.pop().unwrap();
    let lty = typing(lhs, st, p)?;
    let rty = typing(rhs, st, p)?;
    if check_type(&lty.get_type(), &rty.get_type()).is_err() {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &lty.get_type(), &rty.get_type());
    }
    Ok(RRType::new(Type::Void))
}

fn typing_builtin_panic<'a>(_token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => (),
        1 => {
            let format = args.drain(..1).next().unwrap();
            typing(format, st, p)?;
        }
        _ => {
            let format = args.drain(..1).next().unwrap();
            typing(format, st, p)?;
            for arg in args {
                typing(arg, st, p)?;
            }
        }
    }
    Ok(RRType::new(Type::Void))
}

fn typing_builtin_print<'a>(_token: &[Token], st: &mut SymbolTable, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    format_args(_token, st, args, p)
}

fn typing_builtin_println<'a>(_token: &[Token], st: &mut SymbolTable, args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    format_args(_token, st, args, p)
}

fn typing_builtin_read_line<'a>(token: &[Token], _st: &mut SymbolTable, args: Vec<Node>, p: &'a Program) -> Result<RRType> {
    if !args.is_empty() {
        e0000(Rc::clone(&p.errors), (p.path, &p.lines, token), "read_line! takes no arguments");
    }
    Ok(RRType::new(Type::String))
}

fn format_args<'a>(_token: &[Token], st: &mut SymbolTable, mut args: Vec<Node>, p: &'a Program<'a>) -> Result<RRType> {
    let argc = args.len();
    match argc {
        0 => (),
        1 => {
            let format = args.drain(..1).next().unwrap();
            typing(format, st, p)?;
        }
        _ => {
            for arg in args {
                typing(arg, st, p)?;
            }
        }
    }
    Ok(RRType::new(Type::Void))
}
