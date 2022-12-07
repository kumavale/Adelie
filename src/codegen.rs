use crate::ast::*;
use crate::builtin::*;
use crate::error::*;
use crate::keyword::{Type, Numeric, Keyword};
use crate::object::Object;
use crate::program::Program;
use crate::token::Token;
use crate::utils::remove_seq;
use std::rc::Rc;
use std::cell::Ref;

pub fn gen_il<'a>(node: Node, p: &'a Program<'a>) -> Type {
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
        NodeKind::Struct { obj, field } => {
            gen_il_struct(node.token, p, obj.borrow(), field)
        }
        NodeKind::Field { expr, ident } => {
            gen_il_field(node.token, p, *expr, &ident)
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
            gen_il_cast(node.token, p, new_type, *expr)
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

pub fn seq() -> usize {
    unsafe {
        static mut ID: usize = 0;
        ID += 1;
        ID
    }
}

fn gen_il_integer(current_token: &[Token], _p: &Program, ty: Type, num: i128) -> Type {
    use super::token::*;
    debug_assert!(matches!(current_token[0].kind,
            TokenKind::Literal(LiteralKind::Char(_))
            | TokenKind::Literal(LiteralKind::Integer(_))
            | TokenKind::Keyword(Keyword::True)
            | TokenKind::Keyword(Keyword::False)));
    println!("\tldc.i4 {}", num as i32);
    ty
}

fn gen_il_string(_current_token: &[Token], _p: &Program, ty: Type, str: &str) -> Type {
    println!("\tldstr \"{}\"", str);
    ty
}

fn gen_il_box<'a>(_current_token: &[Token], p: &'a Program<'a>, method: Node) -> Type {
    if let NodeKind::Call { name, args } = method.kind {
        match name.as_str() {
            "new" => {
                if args.len() != 1 {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, method.token), 1, args.len());
                }
                let boxed_ty = gen_il(args.into_iter().next().unwrap(), p);
                //println!("\tbox [System.Runtime]System.Int32");
                println!("\tbox {}", boxed_ty.to_ilstr());
                Type::Box(Box::new(boxed_ty))
            }
            _ => {
                e0014(Rc::clone(&p.errors), (p.path, &p.lines, method.token), &name, "Box");
                Type::Void
            }
        }
    } else {
        e0003(Rc::clone(&p.errors), (p.path, &p.lines, method.token));
        Type::Void
    }
}
fn gen_il_call<'a>(current_token: &[Token], p: &'a Program<'a>, name: &str, args: Vec<Node>) -> Type {
    if let Some(func) = p.namespace.borrow().find_fn(name) {
        let params = &func
            .param_symbol_table
            .objs;
        if params.len() != args.len() {
            e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
        }
        for (arg, param) in args.into_iter().zip(params) {
            let token = arg.token;
            let param_ty = &param.borrow().ty;
            let arg_ty = gen_il(arg, p);
            match (&arg_ty, &param_ty) {
                (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => unreachable!(),
                _ if arg_ty == *param_ty => (),
                _ => e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty)
            }
        }
        let params = params
            .iter()
            .map(|p|p.borrow().ty.to_ilstr())
            .collect::<Vec<String>>()
            .join(", ");
        println!("\tcall {} {}({})", func.rettype.to_ilstr(), name, params);
        func.rettype.clone()
    } else {
        e0013(Rc::clone(&p.errors), (p.path, &p.lines, current_token), name);
        Type::Void
    }
}

fn gen_il_method<'a>(current_token: &[Token], p: &'a Program<'a>, expr: Node, ident: &str, args: Vec<Node>) -> Type {
    match gen_il(expr, p) {
        Type::Struct(path, st_name, _) => {
            let ns = p.namespace.borrow();
            let ns = if let Some(ns) = ns.find(&path) {
                ns
            } else {
                let message = format!("failed to resolve: use of undeclared type `{}`", path.join("::"));
                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                return Type::Void;
            };
            if let Some(_st) = ns.find_struct(&st_name) {
                let func = if let Some(func) = ns
                    .impls
                    // TODO: trait毎
                    .first().unwrap()
                    .functions
                    .iter()
                    .find(|f|f.name==ident && !f.is_static)
                {
                    func
                } else {
                    e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, &st_name);
                    return Type::Void;
                };
                let params = &func
                    .param_symbol_table
                    .objs
                    .iter()
                    .skip(if func.is_static { 0 } else { 1 })
                    .collect::<Vec<_>>();
                if params.len() != args.len() {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
                }
                for (arg, param) in args.into_iter().zip(params) {
                    let token = arg.token;
                    let arg_ty = gen_il(arg, p);
                    let param_ty = &param.borrow().ty;
                    match (&arg_ty, &param_ty) {
                        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => unreachable!(),
                        _ if &arg_ty == param_ty => (),
                        _ => e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty)
                    }
                }
                let params = params
                    .iter()
                    .map(|p|p.borrow().ty.to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                println!("\tcall instance {} {}::{}({})", func.rettype.to_ilstr(), st_name, ident, params);
                func.rettype.clone()
            } else {
                // unreachable?
                e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, &st_name);
                Type::Void
            }
        }
        _ => {
            unimplemented!("primitive type")
        }
    }
}

fn gen_il_struct<'a>(current_token: &[Token], p: &'a Program<'a>, obj: Ref<Object>, field: Vec<Node>) -> Type {
    let ns = p.namespace.borrow();
    let ns = if let Type::Struct(path, _, _) = &obj.ty {
        if let Some(ns) = ns.find(path) {
            ns
        } else {
            let message = format!("failed to resolve: use of undeclared type `{}`", path.join("::"));
            e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
            return Type::Void;
        }
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &remove_seq(&obj.name));
        return Type::Void;
    };
    if let Some(st) = ns.find_struct(&obj.ty.to_string()) {
        if field.len() != st.field.len() {
            e0017(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &st.name);
        }
        println!("\tldloca {}", obj.offset);
        println!("\tinitobj {}", obj.ty);
        for (field_expr, field_dec) in field.into_iter().zip(&st.field) {
            println!("\tldloca {}", obj.offset);
            gen_il(field_expr, p);
            println!("\tstfld {} {}::{}", field_dec.ty.to_ilstr(), obj.ty, field_dec.name);
        }
        println!("\tldloc {}", obj.offset);
    } else {
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &remove_seq(&obj.name));
    }
    obj.ty.clone()
}

fn gen_il_field<'a>(current_token: &[Token], p: &'a Program<'a>, expr: Node, ident: &str) -> Type {
    let (path, stname, is_mutable) = match gen_il(expr, p) {
        Type::Struct(path, stname, is_mutable) => {
            (path, stname, is_mutable)
        }
        Type::_Self(path, stname, is_mutable) => {
            //println!("\tldarg.0");
            (path, stname, is_mutable)
        }
        Type::Ptr(ty) => {
            match *ty {
                Type::_Self(path, stname, is_mutable) => {
                    // &self
                    // tmp
                    //println!("\tldarg.0");
                    (path, stname, is_mutable)
                }
                _ => {
                    unimplemented!()
                }
            }
        }
        _ => {
            unimplemented!("primitive type");
        }
    };
    let ns = p.namespace.borrow();
    let ns = if let Some(ns) = ns.find(&path) {
        ns
    } else {
        let message = format!("failed to resolve: use of undeclared type `{}`", path.join("::"));
        e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
        return Type::Void;
    };
    if let Some(st) = ns.find_struct(&stname) {
        if let Some(field) =  st.field.iter().find(|o|o.name==ident) {
            let ty = field.ty.clone();
            match ty {
                Type::Struct(..) => {
                    println!("\tldflda {} {}::{}", ty.to_ilstr(), stname, ident);
                }
                _ => {
                    println!("\tldfld {} {}::{}", ty.to_ilstr(), stname, ident);
                }
            }
            if is_mutable {
                ty.into_mutable()
            } else {
                ty
            }
        } else {
            e0015(Rc::clone(&p.errors), (p.path, &p.lines, current_token), ident, &stname);
            Type::Void
        }
    } else {
        // unreachable?
        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &stname);
        Type::Void
    }
}

fn gen_il_variable(current_token: &[Token], p: &Program, obj: Ref<Object>) -> Type {
    if !obj.assigned {
        e0027(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
    }
    if obj.is_param {
        println!("\tldarg {}", obj.offset);
    } else {
        match obj.ty {
            Type::Struct(..) => {
                println!("\tldloca {}", obj.offset);
            }
            _ => {
                println!("\tldloc {}", obj.offset);
            }
        }
    }
    if obj.is_mutable() {
        obj.ty.clone().into_mutable()
    } else {
        obj.ty.clone()
    }
}

fn gen_il_block<'a>(_current_token: &[Token], p: &'a Program<'a>, stmts: Vec<Node>) -> Type {
    let mut ty = Type::Void;
    for stmt in stmts {
        match stmt.kind {
            NodeKind::Return { .. } => {
                ty = gen_il(stmt, p);
                break;
            }
            _ => {
                ty = gen_il(stmt, p);
            }
        }
    }
    ty
}

fn gen_il_if<'a>(current_token: &[Token], p: &'a Program<'a>, cond: Node, then: Node, els: Option<Box<Node>>) -> Type {
    let token = cond.token;
    let cond_type = gen_il(cond, p);
    if cond_type != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type);
    }
    let seq = seq();
    let else_label = format!("IL_else{}", seq);
    let end_label = format!("IL_end{}", seq);
    println!("\tbrfalse {}", else_label);
    let then_type = gen_il(then, p);
    println!("\tbr {}", end_label);
    println!("{}:", else_label);
    let els_type = els.map(|els| (els.token, gen_il(*els, p)));
    println!("{}:", end_label);
    if let Some(els_type) = els_type {
        match (&then_type, &els_type.1) {
            (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => els_type.1,
            (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => then_type,
            _ if then_type == els_type.1 => then_type,
            _ => {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &then_type, &els_type.1);
                then_type
            }
        }
    } else if then_type != Type::Void {
        e0018(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &then_type);
        Type::Void
    } else {
        then_type
    }
}

fn gen_il_while<'a>(_current_token: &[Token], p: &'a Program<'a>, cond: Node, then: Node, brk_label_seq: usize) -> Type {
    let begin_label = format!("IL_begin{}", seq());
    let end_label = format!("IL_break{}", brk_label_seq);
    println!("{}:", begin_label);
    let token = cond.token;
    let cond_type = gen_il(cond, p);
    if cond_type != Type::Bool {
        e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &cond_type);
    }
    println!("\tbrfalse {}", end_label);
    let then_type = gen_il(then, p);
    println!("\tbr {}", begin_label);
    println!("{}:", end_label);
    then_type
}

fn gen_il_loop<'a>(_current_token: &[Token], p: &'a Program<'a>, then: Node, brk_label_seq: usize) -> Type {
    let begin_label = format!("IL_begin{}", seq());
    let end_label = format!("IL_break{}", brk_label_seq);
    println!("{}:", begin_label);
    let then_type = gen_il(then, p);
    println!("\tbr {}", begin_label);
    println!("{}:", end_label);
    then_type
}

fn gen_il_assign<'a>(current_token: &[Token], p: &'a Program<'a>, lhs: Node, rhs: Node) -> Type {
    match lhs.kind {
        NodeKind::Variable { obj } => {
            fn check_type(lty: &Type, rty: &Type) -> Result<(), ()> {
                match (lty, rty) {
                    (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => Ok(()),
                    (Type::Box(boxedl), Type::Box(boxedr)) => check_type(boxedl, boxedr),
                    _ if lty == rty => Ok(()),
                    _ => Err(())
                }
            }
            let rty = gen_il(rhs, p);
            let is_assigned = obj.borrow().is_assigned();
            obj.borrow_mut().assigned = true;
            let obj = obj.borrow();
            if !obj.is_mutable() && is_assigned {
                e0028(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.name);
            }
            if check_type(&obj.ty, &rty).is_err() {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &obj.ty, &rty);
            }
            if obj.is_param() {
                println!("\tstarg {}", obj.offset);
            } else {
                println!("\tstloc {}", obj.offset);
            }
        }
        NodeKind::UnaryOp { kind: UnaryOpKind::Deref, expr } => {
            let lty = gen_il(*expr, p);
            let rty = gen_il(rhs, p);
            if let Type::Ptr(lty) = lty {
                match (&*lty, &rty) {
                    (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => (),
                    _ if *lty == rty => (),
                    _ => e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty, &rty)
                }
                match *lty {
                    Type::Ptr(_) => println!("\tstind.i"),
                    Type::Numeric(Numeric::I32) => println!("\tstind.i4"),
                    _ => unimplemented!(),
                }
            } else {
                e0022(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &lty);
            }
        }
        NodeKind::Field { expr, ident } => {
            match gen_il(*expr, p) {
                Type::Struct(_, stname, is_mutable) |
                Type::_Self(_, stname, is_mutable) => {
                    if let Some(st) = p.namespace.borrow().find_struct(&stname) {
                        if let Some(field) = st.field.iter().find(|o|o.name==ident) {
                            let rty = gen_il(rhs, p);
                            match (&field.ty, &rty) {
                                (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => (),
                                _ if field.ty == rty => (),
                                _ => e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &field.ty, &rty)
                            }
                            if !is_mutable {
                                let message = format!("cannot assign to `{stname}.{ident}`, as `{stname}` is not declared as mutable");
                                e0000(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &message);
                            }
                            println!("\tstfld {} {}::{}", field.ty.to_ilstr(), stname, ident);
                        } else {
                            e0015(Rc::clone(&p.errors), (p.path, &p.lines, lhs.token), &ident, &stname);
                        }
                    } else {
                        // unreachable?
                        e0016(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &stname);
                    }
                }
                _ => {
                    unimplemented!("primitive type");
                }
            }
        }
        _ => e0019(Rc::clone(&p.errors), (p.path, &p.lines, current_token))
    }
    Type::Void
}

fn gen_il_return<'a>(_current_token: &[Token], p: &'a Program<'a>, expr: Option<Box<Node>>) -> Type {
    let rettype = if let Some(expr) = expr {
        gen_il(*expr, p)
    } else {
        Type::Void
    };
    println!("\tret");
    rettype
}

fn gen_il_break(_current_token: &[Token], _p: &Program, brk_label_seq: usize) -> Type {
    println!("\tbr IL_break{}", brk_label_seq);
    Type::Void
}

fn gen_il_cast<'a>(current_token: &[Token], p: &'a Program<'a>, new_type: Type, expr: Node) -> Type {
    let old_type = gen_il(expr, p);
    match &new_type {
        Type::Numeric(Numeric::I32) => {
            match old_type {
                Type::Numeric(..) | Type::Bool | Type::Char => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Numeric(Numeric::I32)),
            }
            println!("\tconv.i4");
        }
        Type::Bool => {
            match old_type {
                Type::Bool => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Bool),
            }
            println!("\tldc.i4.0");
            println!("\tcgt");
        }
        Type::Char => {
            match old_type {
                Type::Char | Type::Numeric(_) => (),  // ok
                _ => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &Type::Char),
            }
            println!("\tconv.u2");
        }
        Type::Ptr(_) => {
            todo!("cast to ref type");
        }
        Type::Void => unreachable!(),
        t => e0020(Rc::clone(&p.errors), (p.path, &p.lines, current_token), t)
    }
    new_type
}

fn gen_il_unaryop<'a>(current_token: &[Token], p: &'a Program<'a>, kind: UnaryOpKind, expr: Node) -> Type {
    match kind {
        UnaryOpKind::Not => {
            let ty = gen_il(expr, p);
            match ty {
                Type::Bool => {
                    println!("\tldc.i4.0");
                    println!("\tceq");
                }
                _ => println!("\tnot")
            }
            ty
        }
        UnaryOpKind::Neg => {
            let ty= gen_il(expr, p);
            if let Type::Numeric(..) = ty {
                println!("\tneg");
            } else {
                e0021(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ty);
            }
            ty
        }
        UnaryOpKind::Ref => {
            if let NodeKind::Variable { obj } = expr.kind {
                let obj = obj.borrow();
                if obj.is_param {
                    println!("\tldarga {}", obj.offset);
                } else {
                    println!("\tldloca {}", obj.offset);
                }
                Type::Ptr(Box::new(obj.ty.clone()))
            } else {
                Type::Ptr(Box::new(gen_il(expr, p)))
            }
        }
        UnaryOpKind::Deref => {
            let ty = gen_il(expr, p);
            match ty {
                Type::Ptr(ty) => {
                    match *ty {
                        Type::Ptr(_) => println!("\tldind.i"),
                        Type::Numeric(Numeric::I32) => println!("\tldind.i4"),
                        _ => unimplemented!(),
                    }
                    *ty
                }
                Type::Box(ty) => {
                    match *ty {
                        Type::Struct(..) => println!("\tunbox {}", ty.to_ilstr()),
                        _ => println!("\tunbox.any {}", ty.to_ilstr()),
                    }
                    *ty
                }
                _ =>  {
                    e0022(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ty);
                    Type::Void
                }
            }
        }
    }
}

fn gen_il_binaryop<'a>(current_token: &[Token], p: &'a Program<'a>, kind: BinaryOpKind, lhs: Node, rhs: Node) -> Type {
    let ltype = gen_il(lhs, p);
    let rtype = gen_il(rhs, p);
    let mut is_bool    = false;
    let mut is_invalid = false;
    match &ltype {
        Type::Numeric(..) => match kind {
            BinaryOpKind::Add    => println!("\tadd"),
            BinaryOpKind::Sub    => println!("\tsub"),
            BinaryOpKind::Mul    => println!("\tmul"),
            BinaryOpKind::Div    => println!("\tdiv"),
            BinaryOpKind::Rem    => println!("\trem"),
            BinaryOpKind::BitXor => println!("\txor"),
            BinaryOpKind::BitAnd => println!("\tand"),
            BinaryOpKind::BitOr  => println!("\tor"),
            BinaryOpKind::Shl    => println!("\tshl"),
            BinaryOpKind::Shr    => println!("\tshr"),

            BinaryOpKind::Eq => {
                println!("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                println!("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                println!("\tcgt");
                println!("\tldc.i4.0");
                println!("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                println!("\tceq");
                println!("\tldc.i4.0");
                println!("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                println!("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                println!("\tclt");
                println!("\tldc.i4.0");
                println!("\tceq");
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
                is_invalid = true;
            }
            BinaryOpKind::Eq => {
                println!("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                println!("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                println!("\tcgt");
                println!("\tldc.i4.0");
                println!("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                println!("\tceq");
                println!("\tldc.i4.0");
                println!("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                println!("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                println!("\tclt");
                println!("\tldc.i4.0");
                println!("\tceq");
                is_bool = true;
            }
            _ => {
                e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                is_invalid = true;
            }
        }
        Type::String => match kind {
            BinaryOpKind::Add => {
                println!("\tcall string System.String::Concat(string, string)");
            }
            BinaryOpKind::Sub |
            BinaryOpKind::Mul |
            BinaryOpKind::Div |
            BinaryOpKind::Rem => {
                e0023(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                is_invalid = true;
            }
            BinaryOpKind::Eq => {
                println!("\tcall bool System.String::op_Equality(string, string)");
                is_bool = true;
            }
            BinaryOpKind::Lt => {
                println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                println!("\tldc.i4.0");
                println!("\tclt");
                is_bool = true;
            }
            BinaryOpKind::Le => {
                println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                println!("\tldc.i4.0");
                println!("\tcgt");
                println!("\tldc.i4.0");
                println!("\tceq");
                is_bool = true;
            }
            BinaryOpKind::Ne => {
                println!("call bool System.String::op_Inequality(string, string)");
                is_bool = true;
            }
            BinaryOpKind::Gt => {
                println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                println!("\tldc.i4.0");
                println!("\tcgt");
                is_bool = true;
            }
            BinaryOpKind::Ge => {
                println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                println!("\tldc.i4.0");
                println!("\tclt");
                println!("\tldc.i4.0");
                println!("\tceq");
                is_bool = true;
            }
            _ => {
                e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
                is_invalid = true;
            }
        }
        _ => {
            e0024(Rc::clone(&p.errors), (p.path, &p.lines, current_token), kind, &ltype, &rtype);
            is_invalid = true;
        }
    }
    match (&ltype, &rtype) {
        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => {
            if is_bool {
                Type::Bool
            } else {
                rtype
            }
        }
        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => {
            if is_bool {
                Type::Bool
            } else {
                ltype
            }
        }
        _ if ltype == rtype => {
            if is_bool {
                Type::Bool
            } else {
                ltype
            }
        }
        _ => {
            if is_bool {
                Type::Bool
            } else if is_invalid {
                // TODO: これ以上評価したくないので、無効なTypeを返す
                ltype
            } else {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &ltype, &rtype);
                ltype
            }
        }
    }
}

fn gen_il_shortcircuitop<'a>(_current_token: &[Token], p: &'a Program<'a>, kind: ShortCircuitOpKind, lhs: Node, rhs: Node) -> Type {
    let end_label  = format!("IL_end{}", seq());
    match kind {
        ShortCircuitOpKind::And => {
            println!("\tldc.i4.0");
            let token = lhs.token;
            let ltype = gen_il(lhs, p);
            if ltype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype);
            }
            println!("\tbrfalse {}", end_label);
            println!("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, p);
            if rtype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype);
            }
            println!("{}:", end_label);
        }
        ShortCircuitOpKind::Or => {
            println!("\tldc.i4.1");
            let token = lhs.token;
            let ltype = gen_il(lhs, p);
            if ltype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &ltype);
            }
            println!("\tbrtrue {}", end_label);
            println!("\tpop");
            let token = rhs.token;
            let rtype = gen_il(rhs, p);
            if rtype != Type::Bool {
                e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), &Type::Bool, &rtype);
            }
            println!("{}:", end_label);
        }
    }
    Type::Bool
}

fn gen_il_semi<'a>(_current_token: &[Token], p: &'a Program<'a>, expr: Node) -> Type {
    let ty = gen_il(expr, p);
    if ty != Type::Void {
        println!("\tpop");
    }
    ty
}

// WIP
fn gen_il_path<'a>(current_token: &[Token], p: &'a Program<'a>, segment: &str, mut full_path: Vec<String>, child: Node) -> Type {
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
                return Type::Void;
            };
            if let Some(func) = ns.find_fn(&name) {
                let params = &func
                    .param_symbol_table
                    .objs;
                if params.len() != args.len() {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
                }
                for (arg, param) in args.into_iter().zip(params) {
                    let token = arg.token;
                    let param_ty = &param.borrow().ty;
                    let arg_ty = gen_il(arg, p);
                    match (&arg_ty, &param_ty) {
                        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => unreachable!(),
                        _ if arg_ty == *param_ty => (),
                        _ => e0012(Rc::clone(&p.errors), (p.path, &p.lines, token), param_ty, &arg_ty)
                    }
                }
                let params = params
                    .iter()
                    .map(|p|p.borrow().ty.to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                println!("\tcall {} {}({})", func.rettype.to_ilstr(), name, params);
                func.rettype.clone()
            } else if let Some(im) = ns.find_impl(full_path.last().unwrap()) {
                let func = if let Some(func) = im
                    .functions
                    .iter()
                    .find(|f|f.name==name) {
                        func
                    } else {
                        e0014(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &name, &im.name);
                        return Type::Void;
                    };
                let params = &func
                    .param_symbol_table
                    .objs;
                if params.len() != args.len() {
                    e0029(Rc::clone(&p.errors), (p.path, &p.lines, current_token), params.len(), args.len());
                }
                for arg in args {
                    gen_il(arg, p);
                }
                let params = params
                    .iter()
                    .skip(if func.is_static { 0 } else { 1 })
                    .map(|o|o.borrow().ty.to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                if ns.is_foreign {
                    let reference = &ns.name;
                    println!("\tcall {} [{}]System.{}::{}({})", func.rettype.to_ilstr(), reference, segment, name, params);
                    // TODO:                ^^^^^^^
                } else {
                    println!("\tcall {} {}::{}({})", func.rettype.to_ilstr(), segment, name, params);
                }
                func.rettype.clone()
            } else {
                e0013(Rc::clone(&p.errors), (p.path, &p.lines, current_token), &name);
                Type::Void
            }
        }
        _ => {
            gen_il(child, p)
        }
    }
}

fn gen_il_empty() -> Type {
    Type::Void
}
