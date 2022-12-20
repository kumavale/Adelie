use crate::ast::*;
use crate::builtin::*;
use crate::class::ClassKind;
use crate::error::*;
use crate::function::Function;
use crate::keyword::{Type, RRType, Numeric, Keyword};
use crate::namespace::NameSpace;
use crate::object::{FindSymbol, Object, ObjectKind, SymbolTable};
use crate::program::Program;
use crate::token::Token;
use crate::utils::remove_seq;
use std::rc::Rc;
use std::cell::{Ref, RefCell};

type Result<T> = std::result::Result<T, ()>;

pub fn gen_il<'a>(node: Node, p: &'a Program<'a>) -> Result<Type> {
    match node.kind {
        NodeKind::Integer { ty, num } => {
            gen_il_integer(node.token, p, ty, num)
        }
        NodeKind::String { ty, str } => {
            gen_il_string(node.token, p, ty, &str)
        }
        NodeKind::Box { method } => {
            gen_il_box(node.token, p, *method)
        }
        NodeKind::Builtin { kind, args } => {
            gen_il_builtin(node.token, kind, args, p)
        }
        NodeKind::Call { name, args } => {
            gen_il_call(node.token, p, &name, args)
        }
        NodeKind::Method { expr, ident, args } => {
            gen_il_method(node.token, p, *expr, &ident, args)
        }
        NodeKind::Lambda { ty, ident } => {
            gen_il_lambda(node.token, p, ty, &ident)
        }
        NodeKind::Struct { obj, field } => {
            gen_il_struct(node.token, p, obj.borrow(), field)
        }
        NodeKind::FieldOrProperty { lvar_symbol_table, expr, ident } => {
            gen_il_field_or_property(node.token, p, lvar_symbol_table, *expr, &ident)
        }
        NodeKind::Variable { obj } => {
            gen_il_variable(node.token, p, obj.borrow())
        }
        NodeKind::Block { stmts } => {
            gen_il_block(node.token, p, stmts)
        }
        NodeKind::If { cond, then, els } => {
            gen_il_if(node.token, p, *cond, *then, els)
        }
        NodeKind::While { cond, then, brk_label_seq } => {
            gen_il_while(node.token, p, *cond, *then, brk_label_seq)
        }
        NodeKind::Loop { then, brk_label_seq } => {
            gen_il_loop(node.token, p, *then, brk_label_seq)
        }
        NodeKind::Assign { lhs, rhs } => {
            gen_il_assign(node.token, p, *lhs, *rhs)
        }
        NodeKind::Return { expr } => {
            gen_il_return(node.token, p, expr)
        }
        NodeKind::Break { brk_label_seq } => {
            gen_il_break(node.token, p, brk_label_seq)
        }
        NodeKind::Cast { ty: new_type, expr } => {
            gen_il_cast(node.token, p, new_type.borrow().clone(), *expr)
        }
        NodeKind::UnaryOp { kind, expr } => {
            gen_il_unaryop(node.token, p, kind, *expr)
        }
        NodeKind::BinaryOp { kind, lhs, rhs } => {
            gen_il_binaryop(node.token, p, kind, *lhs, *rhs)
        }
        NodeKind::ShortCircuitOp { kind, lhs, rhs } => {
            gen_il_shortcircuitop(node.token, p, kind, *lhs, *rhs)
        }
        NodeKind::Semi { expr } => {
            gen_il_semi(node.token, p, *expr)
        }
        NodeKind::Path { segment, child } => {
            gen_il_path(node.token, p, &segment, vec![segment.to_string()], *child)
        }
        NodeKind::Empty => {
            gen_il_empty()
        }
    }
}

pub fn label_seq() -> usize {
    unsafe {
        static mut ID: usize = 0;
        ID += 1;
        ID
    }
}

fn gen_il_integer(current_token: &[Token], p: &Program, ty: Type, num: i128) -> Result<Type> {
    use super::token::*;
    debug_assert!(matches!(current_token[0].kind,
            TokenKind::Literal(LiteralKind::Char(_))
            | TokenKind::Literal(LiteralKind::Integer(_))
            | TokenKind::Keyword(Keyword::True)
            | TokenKind::Keyword(Keyword::False)));
    p.push_il(format!("\tldc.i4 {}", num as i32));
    Ok(ty)
}

fn gen_il_string(_current_token: &[Token], p: &Program, ty: Type, str: &str) -> Result<Type> {
    p.push_il(format!("\tldstr \"{}\"", str));
    Ok(ty)
}

fn gen_il_box<'a>(_current_token: &[Token], p: &'a Program<'a>, method: Node) -> Result<Type> {
    if let NodeKind::Call { name, args } = method.kind {
        match name.as_str() {
            "new" => {
                if args.len() != 1 {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, method.token), 1, args.len());
                }
                let boxed_ty = gen_il(args.into_iter().next().unwrap(), p)?;
                //println!("\tbox [System.Runtime]System.Int32");
                p.push_il(format!("\tbox {}", boxed_ty.to_ilstr()));
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
fn gen_il_call<'a>(current_token: &[Token], p: &'a Program<'a>, name: &str, args: Vec<Node>) -> Result<Type> {
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
        p.push_il(format!("\tcall {} '{}'::'{}'({})", func.rettype.borrow().to_ilstr(), p.name, name, params));
        Ok(func.rettype.borrow().clone())
    } else {
        e0013(Rc::clone(&p.errors), (p.path, &p.lines, current_token), name);
        Err(())
    }
}

fn gen_il_method<'a>(
    current_token: &[Token],
    p: &'a Program<'a>,
    expr: Node,
    ident: &str,
    args: Vec<Node>,
) -> Result<Type> {
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
                                .find_impl(&name)
                                .and_then(|im| im.functions.find(ident).map(Rc::clone))
                                .and_then(|f| (!f.is_static).then_some(f)) {
                                    Some(func)
                            } else {
                                find_func_recursive(ns, &base, ident)
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
                    .skip(if func.is_static { 0 } else { 1 })
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
                p.push_il(format!("\tcall instance {} {}::'{}'({})", func.rettype.borrow().to_ilstr(), parent_ty.to_ilstr(), ident, params));
                let x = Ok(func.rettype.borrow().clone());
                x
            } else {
                e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, cl_name);
                Err(())
            }
        }
        // 仮実装
        Type::Numeric(Numeric::I32) if ident == "to_string" => {
            p.push_il(format!("\tcall instance string {}::ToString()", parent_ty.to_ilstr()));
            Ok(Type::String)
        }
        ty => {
            p.errors.borrow().display();
            unimplemented!("primitive type: {:?}", ty);
        }
    }
}

fn gen_il_lambda<'a>(
    current_token: &[Token],
    p: &'a Program<'a>,
    ty: Type,
    ident: &str,
) -> Result<Type> {
    //println!("\tldc.i4.0");  // 本当は`sender`のobjectをロードする必要がある？
    //println!("\tldftn instance void '{}'()", ident);  // インターナルclass内に定義していないから`instance`は要らない
    // めっちゃ強引に書いているだけ
    //println!("\tldftn void Form1/'<>c__DisplayClass0_0'::'{}'()", ident);
    p.push_il(format!("\tldloc '{}'", format!("<main>nested_class")));
    //let end_label = format!("\tIL_lambda_ctor_end{}", crate::seq!());
    //println!("\tbrtrue {}", end_label);
    //println!("\tnewobj instance void [System.Runtime]System.Object::.ctor()");
    //println!("\tstloc '{}'", format!("<main>nested_class"));
    //println!("\tldloc '{}'", format!("<main>nested_class"));
    //println!("{}:", end_label);
    p.push_il(format!("\tldftn instance void '{}'/'<>c__DisplayClass0_0'::'{}'()", p.name, ident));
    p.push_il("\tnewobj instance void [mscorlib]System.EventHandler::.ctor(object, native int)");
    Ok(ty)
}

fn gen_il_struct<'a>(current_token: &[Token], p: &'a Program<'a>, obj: Ref<Object>, field: Vec<Node>) -> Result<Type> {
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
        p.push_il(format!("\tldloca {}", obj.offset));
        p.push_il(format!("\tinitobj {}", obj.ty.borrow()));
        for (field_expr, field_dec) in field.into_iter().zip(&st.borrow().field.objs) {
            p.push_il(format!("\tldloca {}", obj.offset));
            gen_il(field_expr, p)?;
            p.push_il(format!("\tstfld {} {}::'{}'", field_dec.borrow().ty.borrow().to_ilstr(), obj.ty.borrow(), field_dec.borrow().name));
        }
        p.push_il(format!("\tldloc {}", obj.offset));
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &remove_seq(&obj.name));
    }
    Ok(obj.ty.borrow().clone())
}

fn gen_il_field_or_property<'a>(
    current_token: &[Token],
    p: &'a Program<'a>,
    lvar_symbol_table: Rc<RefCell<SymbolTable>>,
    expr: Node,
    ident: &str,
) -> Result<Type> {
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
                    unimplemented!()
                }
            }
        }
        ty => {
            unimplemented!("primitive type: {:?}", ty);
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
                p.push_il(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
            } else if *p.ret_address.borrow() {
                p.push_il(format!("\tldflda {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
            } else {
                p.push_il(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
            }
            if is_mutable {
                Ok(field.borrow().ty.borrow().clone().into_mutable())
            } else {
                Ok(field.borrow().ty.borrow().clone())
            }
        } else if let Some(property) = cl.borrow().properties.iter().find(|o|o.name==ident) {
            if let Type::Class(ClassKind::Struct, ..) = parent_ty {
                // parent_tyを`stloc`するための一意なローカル変数を定義する
                // スタックのトップには`struct`の'値'が入っている
                // それをローカル変数に格納して、`ldloca`でアドレスをスタックに積む
                let unique_name = format!("{}:{}", parent_ty, crate::seq!());
                let obj = Object::new(unique_name,
                    lvar_symbol_table.borrow().offset(ObjectKind::Local),
                    ObjectKind::Local,
                    RRType::new(parent_ty.clone()),
                    true);
                p.push_il(format!("\tstloc {}", obj.offset));
                p.push_il(format!("\tldloca {}", obj.offset));
                lvar_symbol_table.borrow_mut().push(Rc::new(RefCell::new(obj)));
            }
            let method_name = format!("get_{}", ident);
            p.push_il(format!("\tcall instance {} {}::'{}'()", property.ty.borrow().to_ilstr(), parent_ty.to_ilstr(), method_name));
            if is_mutable {
                Ok(property.ty.borrow().clone().into_mutable())
            } else {
                Ok(property.ty.borrow().clone())
            }
        } else {
            // baseクラスから検索
            fn search_from_base(base_ty: Option<RRType>, parent_ty: &Type, p: &Program, lvar_symbol_table: Rc<RefCell<SymbolTable>>, ident: &str, is_mutable: bool) -> Result<Type> {
                if let Some(base_ty) = base_ty {
                    let ns = p.namespace.borrow();
                    let base_ty = base_ty.borrow();
                    let (path, name, base)  = if let Type::Class(_, _, p, n, b, ..) = &*base_ty { (p, n, b) } else { unreachable!() };
                    if let Some(ns) = ns.find(path) {
                        if let Some(cl) = ns.find_class(|_|true, name) {
                            if let Some(field) = cl.borrow().field.find(ident) {
                                if let Type::Class(ClassKind::Class, ..) = *field.borrow().ty.borrow()  {
                                    p.push_il(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                                } else if *p.ret_address.borrow() {
                                    p.push_il(format!("\tldflda {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), base_ty.to_ilstr(), ident));
                                } else {
                                    p.push_il(format!("\tldfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), base_ty.to_ilstr(), ident));
                                }
                                if is_mutable {
                                    return Ok(field.borrow().ty.borrow().clone().into_mutable());
                                } else {
                                    return Ok(field.borrow().ty.borrow().clone());
                                }
                            } else if let Some(property) = cl.borrow().properties.iter().find(|o|o.name==ident) {
                                if let Type::Class(ClassKind::Struct, ..) = *parent_ty {
                                    // parent_tyを`stloc`するための一意なローカル変数を定義する
                                    // スタックのトップには`struct`の'値'が入っている
                                    // それをローカル変数に格納して、`ldloca`でアドレスをスタックに積む
                                    let unique_name = format!("{}:{}", base_ty, crate::seq!());
                                    let obj = Object::new(unique_name,
                                        lvar_symbol_table.borrow().offset(ObjectKind::Local),
                                        ObjectKind::Local,
                                        RRType::new(base_ty.clone()),
                                        true);
                                    p.push_il(format!("\tstloc {}", obj.offset));
                                    p.push_il(format!("\tldloca {}", obj.offset));
                                    lvar_symbol_table.borrow_mut().push(Rc::new(RefCell::new(obj)));
                                }
                                let method_name = format!("get_{}", ident);
                                p.push_il(format!("\tcall instance {} {}::'{}'()", property.ty.borrow().to_ilstr(), base_ty.to_ilstr(), method_name));
                                if is_mutable {
                                    return Ok(property.ty.borrow().clone().into_mutable());
                                } else {
                                    return Ok(property.ty.borrow().clone());
                                }
                            }
                        }
                    }
                    return search_from_base(base.clone(), parent_ty, p, Rc::clone(&lvar_symbol_table), ident, is_mutable);
                }
                Err(())
            }
            search_from_base(base, &parent_ty, p, Rc::clone(&lvar_symbol_table), ident, is_mutable)
                .map_err(|()| e0015(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, &parent_name))
        }
    } else {
        // unreachable?
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &parent_name);
        Err(())
    }
}

fn gen_il_variable(current_token: &[Token], p: &Program, obj: Ref<Object>) -> Result<Type> {
    if !obj.assigned {
        // TODO: objのis_assignedを再帰的にtrueにする必要がある
        dbg!(&obj);
        e0027(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
    }
    if obj.is_param() {
        p.push_il(format!("\tldarg {}", obj.offset));
    } else if *p.ret_address.borrow() {
        p.push_il(format!("\tldloca {}", obj.offset));
    } else {
        p.push_il(format!("\tldloc {}", obj.offset));
    }
    if obj.is_mutable() {
        Ok(obj.ty.borrow().clone().into_mutable())
    } else {
        Ok(obj.ty.borrow().clone())
    }
}

fn gen_il_block<'a>(_current_token: &[Token], p: &'a Program<'a>, stmts: Vec<Node>) -> Result<Type> {
    let mut ty = Type::Void;
    for stmt in stmts {
        match stmt.kind {
            NodeKind::Return { .. } => {
                ty = gen_il(stmt, p)?;
                break;
            }
            _ => {
                ty = gen_il(stmt, p)?;
            }
        }
    }
    Ok(ty)
}

fn gen_il_if<'a>(current_token: &[Token], p: &'a Program<'a>, cond: Node, then: Node, els: Option<Box<Node>>) -> Result<Type> {
    fn check_type(then: &Type, els: &Type) -> Result<Type> {
        match (then, els) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => Ok(els.clone()),
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(then.clone()),
            (Type::Box(l), Type::Box(r)) |
            (Type::Ptr(l), Type::Ptr(r)) => check_type(&l.borrow(), &r.borrow()),
            _ if then == els => Ok(then.clone()),
            _ => Err(())
        }
    }
    let token = cond.token;
    let cond_type = gen_il(cond, p)?;
    if cond_type != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type);
    }
    let seq = label_seq();
    let else_label = format!("IL_else{}", seq);
    let end_label = format!("IL_end{}", seq);
    p.push_il(format!("\tbrfalse {}", else_label));
    let then_type = gen_il(then, p)?;
    p.push_il(format!("\tbr {}", end_label));
    p.push_il(format!("{}:", else_label));
    let els = els.map(|els| (els.token, gen_il(*els, p)));
    p.push_il(format!("{}:", end_label));
    if let Some(els) = els {
        let els_token = els.0;
        let els_type = els.1?;
        check_type(&then_type, &els_type)
            .map_err(|()| {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, els_token), &then_type, &els_type);
            })
    } else if then_type != Type::Void {
        e0018(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &then_type);
        Err(())
    } else {
        Ok(then_type)
    }
}

fn gen_il_while<'a>(_current_token: &[Token], p: &'a Program<'a>, cond: Node, then: Node, brk_label_seq: usize) -> Result<Type> {
    let begin_label = format!("IL_begin{}", label_seq());
    let end_label = format!("IL_break{}", brk_label_seq);
    p.push_il(format!("{}:", begin_label));
    let token = cond.token;
    let cond_type = gen_il(cond, p)?;
    if cond_type != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type);
    }
    p.push_il(format!("\tbrfalse {}", end_label));
    let then_type = gen_il(then, p);
    p.push_il(format!("\tbr {}", begin_label));
    p.push_il(format!("{}:", end_label));
    then_type
}

fn gen_il_loop<'a>(_current_token: &[Token], p: &'a Program<'a>, then: Node, brk_label_seq: usize) -> Result<Type> {
    let begin_label = format!("IL_begin{}", label_seq());
    let end_label = format!("IL_break{}", brk_label_seq);
    p.push_il(format!("{}:", begin_label));
    let then_type = gen_il(then, p);
    p.push_il(format!("\tbr {}", begin_label));
    p.push_il(format!("{}:", end_label));
    then_type
}

fn gen_il_assign<'a>(current_token: &[Token], p: &'a Program<'a>, lhs: Node, rhs: Node) -> Result<Type> {
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
                p.push_il(format!("\tstarg {}", obj.offset));
            } else {
                p.push_il(format!("\tstloc {}", obj.offset));
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
                Type::Ptr(_) => p.push_il("\tstind.i"),
                Type::Numeric(Numeric::I32) => p.push_il("\tstind.i4"),
                _ => unimplemented!(),
            }
        }
        NodeKind::FieldOrProperty { lvar_symbol_table: _, expr, ident } => {
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
                    if let Some(st) = ns.find_class(|k|k==&ClassKind::Struct, name) {
                        if let Some(field) = st.borrow().field.find(&ident) {
                            let rty = gen_il(rhs, p)?;
                            if check_type(&field.borrow().ty.borrow(), &rty).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.borrow().ty.borrow(), &rty);
                            }
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
                            p.push_il(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        } else if let Some(property) = st.borrow().properties.iter().find(|o|o.name==ident) {
                            let rty = gen_il(rhs, p)?;
                            if check_type(&property.ty.borrow(), &rty).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &property.ty.borrow(), &rty);
                            }
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
                            let method_name = format!("set_{}", ident);
                            p.push_il(format!("\tcall instance void {}::'{}'({})", parent_ty.to_ilstr(), method_name, property.ty.borrow().to_ilstr()));
                        } else {
                            e0015(Rc::clone(&p.errors), (p.path, &p.lines, lhs.token), &ident, name);
                        }
                    } else {
                        // unreachable?
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
                            p.push_il(format!("\tstfld {} {}::'{}'", field.borrow().ty.borrow().to_ilstr(), parent_ty.to_ilstr(), ident));
                        } else if let Some(property) = cl.borrow().properties.iter().find(|o|o.name==ident) {
                            let rty = gen_il(rhs, p)?;
                            if check_type(&property.ty.borrow(), &rty).is_err() {
                                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &property.ty.borrow(), &rty);
                            }
                            if !is_mutable {
                                let message = format!("cannot assign to `{name}.{ident}`, as `{name}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
                            let method_name = format!("set_{}", ident);
                            p.push_il(format!("\tcall instance void {}::'{}'({})", parent_ty.to_ilstr(), method_name, property.ty.borrow().to_ilstr()));
                        } else {
                            e0015(Rc::clone(&p.errors), (p.path, &p.lines, lhs.token), &ident, name);
                        }
                    } else {
                        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), name);
                    }
                }
                ty => {
                    unimplemented!("primitive type: {:?}", ty);
                }
            }
        }
        _ => e0019(Rc::clone(&p.errors), (p.path, &p.lines, current_token))
    }
    Ok(Type::Void)
}

fn gen_il_return<'a>(_current_token: &[Token], p: &'a Program<'a>, expr: Option<Box<Node>>) -> Result<Type> {
    let rettype = if let Some(expr) = expr {
        gen_il(*expr, p)?
    } else {
        Type::Void
    };
    p.push_il("\tret");
    Ok(rettype)
}

fn gen_il_break(_current_token: &[Token], p: &Program, brk_label_seq: usize) -> Result<Type> {
    p.push_il(format!("\tbr IL_break{}", brk_label_seq));
    Ok(Type::Void)
}

fn gen_il_cast<'a>(current_token: &[Token], p: &'a Program<'a>, new_type: Type, expr: Node) -> Result<Type> {
    let old_type = gen_il(expr, p)?;
    match &new_type {
        Type::Numeric(Numeric::I32) => {
            match old_type {
                Type::Numeric(..) | Type::Bool | Type::Char => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Numeric(Numeric::I32)),
            }
            p.push_il("\tconv.i4");
        }
        Type::Bool => {
            match old_type {
                Type::Bool => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Bool),
            }
            p.push_il("\tldc.i4.0");
            p.push_il("\tcgt");
        }
        Type::Char => {
            match old_type {
                Type::Char | Type::Numeric(_) => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Char),
            }
            p.push_il("\tconv.u2");
        }
        Type::Ptr(_) => {
            todo!("cast to ref type");
        }
        Type::Void => unreachable!(),
        t => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), t)
    }
    Ok(new_type)
}

fn gen_il_unaryop<'a>(current_token: &[Token], p: &'a Program<'a>, kind: UnaryOpKind, expr: Node) -> Result<Type> {
    match kind {
        UnaryOpKind::Not => {
            let ty = gen_il(expr, p)?;
            match ty {
                Type::Bool => {
                    p.push_il("\tldc.i4.0");
                    p.push_il("\tceq");
                }
                _ => p.push_il("\tnot")
            }
            Ok(ty)
        }
        UnaryOpKind::Neg => {
            let ty= gen_il(expr, p)?;
            if let Type::Numeric(..) = ty {
                p.push_il("\tneg");
            } else {
                e0021(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ty);
            }
            Ok(ty)
        }
        UnaryOpKind::Ref => {
            if let NodeKind::Variable { obj } = expr.kind {
                let obj = obj.borrow();
                if obj.is_param() {
                    p.push_il(format!("\tldarga {}", obj.offset));
                } else {
                    p.push_il(format!("\tldloca {}", obj.offset));
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
                        Type::Ptr(_) => p.push_il("\tldind.i"),
                        Type::Numeric(Numeric::I32) => p.push_il("\tldind.i4"),
                        _ => unimplemented!(),
                    }
                    Ok(ty)
                }
                Type::Box(ty) => {
                    let ty = ty.borrow().clone();
                    match ty {
                        Type::Class(ClassKind::Struct, ..) => p.push_il(format!("\tunbox {}", ty.to_ilstr())),
                        _ => p.push_il(format!("\tunbox.any {}", ty.to_ilstr())),
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

fn gen_il_binaryop<'a>(current_token: &[Token], p: &'a Program<'a>, kind: BinaryOpKind, lhs: Node, rhs: Node) -> Result<Type> {
    let ltype = gen_il(lhs, p)?;
    let rtype = gen_il(rhs, p)?;
    let mut is_bool    = false;
    match &ltype {
        Type::Numeric(..) => match kind {
            BinaryOpKind::Add    => p.push_il("\tadd"),
            BinaryOpKind::Sub    => p.push_il("\tsub"),
            BinaryOpKind::Mul    => p.push_il("\tmul"),
            BinaryOpKind::Div    => p.push_il("\tdiv"),
            BinaryOpKind::Rem    => p.push_il("\trem"),
            BinaryOpKind::BitXor => p.push_il("\txor"),
            BinaryOpKind::BitAnd => p.push_il("\tand"),
            BinaryOpKind::BitOr  => p.push_il("\tor"),
            BinaryOpKind::Shl    => p.push_il("\tshl"),
            BinaryOpKind::Shr    => p.push_il("\tshr"),

            BinaryOpKind::Eq => {
                p.push_il("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                p.push_il("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                p.push_il("\tcgt");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                p.push_il("\tceq");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                p.push_il("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                p.push_il("\tclt");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
                is_bool = true;
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
                p.push_il("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                p.push_il("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                p.push_il("\tcgt");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                p.push_il("\tceq");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                p.push_il("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                p.push_il("\tclt");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
                is_bool = true;
            }
            _ => {
                e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
        }
        Type::String => match kind {
            BinaryOpKind::Add => {
                p.push_il("\tcall string System.String::Concat(string, string)");
            }
            BinaryOpKind::Sub |
            BinaryOpKind::Mul |
            BinaryOpKind::Div |
            BinaryOpKind::Rem => {
                e0023(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                return Err(());
            }
            BinaryOpKind::Eq => {
                p.push_il("\tcall bool System.String::op_Equality(string, string)");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                p.push_il("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il("\tldc.i4.0");
                p.push_il("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                p.push_il("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il("\tldc.i4.0");
                p.push_il("\tcgt");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                p.push_il("call bool System.String::op_Inequality(string, string)");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                p.push_il("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il("\tldc.i4.0");
                p.push_il("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                p.push_il("\tcallvirt instance int32 System.String::CompareTo(string)");
                p.push_il("\tldc.i4.0");
                p.push_il("\tclt");
                p.push_il("\tldc.i4.0");
                p.push_il("\tceq");
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

fn gen_il_shortcircuitop<'a>(_current_token: &[Token], p: &'a Program<'a>, kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Result<Type> {
    let end_label  = format!("IL_end{}", label_seq());
    match kind {
        ShortCircuitOpKind::And => {
            p.push_il("\tldc.i4.0");
            let token = lhs.token;
            let ltype = gen_il(lhs, p)?;
            if ltype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype);
            }
            p.push_il(format!("\tbrfalse {}", end_label));
            p.push_il("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, p)?;
            if rtype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype);
            }
            p.push_il(format!("{}:", end_label));
        }
        ShortCircuitOpKind::Or => {
            p.push_il("\tldc.i4.1");
            let token = lhs.token;
            let ltype = gen_il(lhs, p)?;
            if ltype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype);
            }
            p.push_il(format!("\tbrtrue {}", end_label));
            p.push_il("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, p)?;
            if rtype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype);
            }
            p.push_il(format!("{}:", end_label));
        }
    }
    Ok(Type::Bool)
}

fn gen_il_semi<'a>(_current_token: &[Token], p: &'a Program<'a>, expr: Node) -> Result<Type> {
    let ty = gen_il(expr, p)?;
    if ty != Type::Void {
        p.push_il("\tpop");
    }
    Ok(Type::Void)
}

// WIP
fn gen_il_path<'a>(current_token: &[Token], p: &'a Program<'a>, segment: &str, mut full_path: Vec<String>, child: Node) -> Result<Type> {
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
                p.push_il(format!("\tcall {} '{}'::'{}'({})", func.rettype.borrow().to_ilstr(), p.name, name, params));
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
                    .skip(if func.is_static { 0 } else { 1 })
                    .map(|o|o.borrow().ty.borrow().to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                if ns.is_foreign {
                    let reference = &im.reference.as_ref().unwrap();
                    if func.is_ctor {
                        p.push_il(format!("\tnewobj instance void [{}]{}::'{}'({})", reference, full_path.join("."), name, params));
                    } else {
                        p.push_il(format!("\tcall {} [{}]{}::'{}'({})", func.rettype.borrow().to_ilstr(), reference, full_path.join("."), name, params));
                    }
                } else {
                    p.push_il(format!("\tcall {} {}::'{}'({})", func.rettype.borrow().to_ilstr(), segment, name, params));
                }
                Ok(func.rettype.borrow().clone())
            } else {
                e0013(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &name);
                Err(())
            }
        }
        _ => {
            gen_il(child, p)
        }
    }
}

fn gen_il_empty() -> Result<Type> {
    Ok(Type::Void)
}
