use super::ast::*;
use super::builtin::*;
use super::keyword::*;
use super::program::*;

pub fn gen_il(node: Node, p: &Program) -> Type {
    match node.kind {
        NodeKind::Integer { ty, num } => {
            println!("\tldc.i4 {}", num as i32);
            ty
        }
        NodeKind::String { ty, str } => {
            println!("\tldstr \"{}\"", str);
            ty
        }
        NodeKind::Builtin { kind, args } => {
            gen_builtin_il(kind, args, p)
        }
        NodeKind::Call { name, args } => {
            if let Some(func) = p.find_fn(&name) {
                let params = func
                    .param_symbol_table
                    .objs
                    .iter()
                    .map(|o|&o.ty)
                    .collect::<Vec<&Type>>();
                for (arg, param_ty) in args.into_iter().zip(&params) {
                    let arg_ty = gen_il(arg, p);
                    match (&arg_ty, &param_ty) {
                        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => unreachable!(),
                        _ if &arg_ty == *param_ty => (),
                        _ => panic!("expected `{}`, found `{}`", arg_ty, param_ty)
                    }
                }
                let params = params
                    .iter()
                    .map(|p|p.to_ilstr())
                    .collect::<Vec<String>>()
                    .join(", ");
                println!("\tcall {} {}({})", func.rettype.to_ilstr(), name, params);
                func.rettype.clone()
            } else {
                panic!("The name '{}' does not exist in the current context", name);
            }
        }
        NodeKind::Method { expr, ident, args } => {
            match gen_il(*expr, p) {
                Type::Struct(st) => {
                    if let Some(im) = p.find_impl(&st) {
                        let func = im
                            .functions
                            .iter()
                            .find(|f|f.name==ident && !f.is_static)
                            .unwrap_or_else(|| panic!("The name '{}' does not exist in the current context", ident));
                        let params = func
                            .param_symbol_table
                            .objs
                            .iter()
                            .skip(if func.is_static { 0 } else { 1 })
                            .map(|o|&o.ty)
                            .collect::<Vec<&Type>>();
                        for (arg, param_ty) in args.into_iter().zip(&params) {
                            let arg_ty = gen_il(arg, p);
                            match (&arg_ty, &param_ty) {
                                (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                                (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => unreachable!(),
                                _ if &arg_ty == *param_ty => (),
                                _ => panic!("expected `{}`, found `{}`", arg_ty, param_ty)
                            }
                        }
                        let params = params
                            .iter()
                            .map(|p|p.to_ilstr())
                            .collect::<Vec<String>>()
                            .join(", ");
                        println!("\tcall instance {} {}::{}({})", func.rettype.to_ilstr(), st, ident, params);
                        func.rettype.clone()
                    } else {
                        panic!("The name '{}' does not exist in the current context", ident);
                    }
                }
                _ => {
                    unimplemented!("primitive type")
                }
            }
        }
        NodeKind::Struct { obj, field } => {
            if let Some(st) = p.find_struct(&obj.ty.to_string()) {
                if field.len() != st.field.len() {
                    panic!("missing field");
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
                panic!("The name '{}' does not exist in the current context", obj.name);
            }
            Type::Struct(obj.ty.to_string())
        }
        NodeKind::Field { expr, ident } => {
            let stname = match gen_il(*expr, p) {
                Type::Struct(stname) => {
                    stname
                }
                Type::_Self(stname) => {
                    //println!("\tldarg.0");
                    stname
                }
                Type::Ptr(ty) => {
                    match *ty {
                        Type::_Self(stname) => {
                            // &self
                            // tmp
                            //println!("\tldarg.0");
                            stname
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
            if let Some(st) = p.find_struct(&stname) {
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
                    ty
                } else {
                    panic!("no field `{}` on type `{}`", ident, stname);
                }
            } else {
                panic!("cannot find value `{}` in this scope", stname);
            }
        }
        NodeKind::Variable { obj } => {
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
            obj.ty.clone()
        }
        NodeKind::Block { stmts } => {
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
        NodeKind::If { cond, then, els } => {
            let cond_type = gen_il(*cond, p);
            if cond_type != Type::Bool {
                panic!("expected `{}`, found `{}`", Type::Bool, cond_type);
            }
            let seq = seq();
            let else_label = format!("IL_else{}", seq);
            let end_label = format!("IL_end{}", seq);
            println!("\tbrfalse {}", else_label);
            let then_type = gen_il(*then, p);
            println!("\tbr {}", end_label);
            println!("{}:", else_label);
            let els_type = els.map(|els| gen_il(*els, p));
            println!("{}:", end_label);
            if let Some(els_type) = els_type {
                match (&then_type, &els_type) {
                    (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => els_type,
                    (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => then_type,
                    _ if then_type == els_type => then_type,
                    _ => panic!("expected `{}`, found `{}`", then_type, els_type)
                }
            } else if then_type != Type::Void {
                eprintln!("expect `()`, found `{}`", then_type);
                panic!("`if` may be missing an `else` clause")
            } else {
                then_type
            }
        }
        NodeKind::While { cond, then, brk_label_seq } => {
            let begin_label = format!("IL_begin{}", seq());
            let end_label = format!("IL_break{}", brk_label_seq);
            println!("{}:", begin_label);
            let cond_type = gen_il(*cond, p);
            if cond_type != Type::Bool {
                panic!("expected `{}`, found `{}`", Type::Bool, cond_type);
            }
            println!("\tbrfalse {}", end_label);
            let then_type = gen_il(*then, p);
            println!("\tbr {}", begin_label);
            println!("{}:", end_label);
            then_type
        }
        NodeKind::Loop { then, brk_label_seq } => {
            let begin_label = format!("IL_begin{}", seq());
            let end_label = format!("IL_break{}", brk_label_seq);
            println!("{}:", begin_label);
            let then_type = gen_il(*then, p);
            println!("\tbr {}", begin_label);
            println!("{}:", end_label);
            then_type
        }
        NodeKind::Assign { lhs, rhs } => {
            match lhs.kind {
                NodeKind::Variable { obj } => {
                    gen_il(*rhs, p);
                    println!("\tstloc {}", obj.offset);
                }
                NodeKind::UnaryOp { kind: UnaryOpKind::Deref, expr } => {
                    //match expr {
                    //    NodeKind::Variable { obj }
                    //}
                    gen_il(*expr, p);
                    gen_il(*rhs, p);
                    println!("\tstind.i4");
                }
                NodeKind::Field { expr, ident } => {
                    if let Type::Struct(tag) = gen_il(*expr, p) {
                        if let Some(st) = p.find_struct(&tag) {
                            if let Some(field) = st.field.iter().find(|o|o.name==ident) {
                                gen_il(*rhs, p);
                                println!("\tstfld {} {}::{}", field.ty.to_ilstr(), tag, ident);
                            } else {
                                panic!("no field `{}` on type `{}`", ident, tag);
                            }
                        } else {
                            panic!("cannot find value `{}` in this scope", tag);
                        }
                    } else {
                        unimplemented!("primitive type");
                    }
                }
                _ => panic!("The left-hand side of an assignment must be a variable")
            }
            Type::Void
        }
        NodeKind::Return { expr } => {
            let rettype = if let Some(expr) = expr {
                gen_il(*expr, p)
            } else {
                Type::Void
            };
            println!("\tret");
            rettype
        }
        NodeKind::Break { brk_label_seq } => {
            println!("\tbr IL_break{}", brk_label_seq);
            Type::Void
        }
        NodeKind::Cast { ty: new_type, expr } => {
            let old_type = gen_il(*expr, p);
            match new_type {
                Type::Numeric(Numeric::I32) => {
                    println!("\tconv.i4");
                }
                Type::Bool => {
                    if let Type::Numeric(_) = old_type {
                        panic!("invalid cast as `{}`", Type::Bool);
                    }
                    println!("\tldc.i4.0");
                    println!("\tcgt");
                }
                Type::Char => {
                    println!("\tconv.u2");
                }
                Type::Ptr(_) => {
                    todo!("cast to ref type");
                }
                Type::Void => unreachable!(),
                t => panic!("invalid cast as `{}`", t),
            }
            new_type
        }
        NodeKind::UnaryOp { kind, expr } => {
            match kind {
                UnaryOpKind::Not => {
                    let ty = gen_il(*expr, p);
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
                    let ty= gen_il(*expr, p);
                    println!("\tneg");
                    ty
                }
                UnaryOpKind::Ref => {
                    if let NodeKind::Variable { obj } = expr.kind {
                        if obj.is_param {
                            println!("\tldarga {}", obj.offset);
                        } else {
                            println!("\tldloca {}", obj.offset);
                        }
                        Type::Ptr(Box::new(obj.ty.clone()))
                    } else {
                        Type::Ptr(Box::new(gen_il(*expr, p)))
                    }
                }
                UnaryOpKind::Deref => {
                    let ty = gen_il(*expr, p);
                    if let Type::Ptr(ty) = ty{
                        match *ty {
                            Type::Ptr(_) => println!("\tldind.i"),
                            Type::Numeric(Numeric::I32) => println!("\tldind.i4"),
                            _ => unimplemented!(),
                        }
                        *ty
                    } else {
                        panic!("type `{}` cannot be dereferenced", ty);
                    }
                }
            }
        }
        NodeKind::BinaryOp { kind, lhs, rhs } => {
            let ltype = gen_il(*lhs, p);
            let rtype = gen_il(*rhs, p);
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
                        return Type::Bool;
                    }
                    BinaryOpKind::Lt => {
                        println!("\tclt");
                        return Type::Bool;
                    }
                    BinaryOpKind::Le => {
                        println!("\tcgt");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                    BinaryOpKind::Ne => {
                        println!("\tceq");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                    BinaryOpKind::Gt => {
                        println!("\tcgt");
                        return Type::Bool;
                    }
                    BinaryOpKind::Ge => {
                        println!("\tclt");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                }
                Type::Char | Type::Bool => match kind {
                    BinaryOpKind::Add => {
                        panic!("cannot add `{}` to `{}`", ltype, rtype);
                    }
                    BinaryOpKind::Sub => {
                        panic!("cannot subtract `{}` from `{}`", ltype, rtype);
                    }
                    BinaryOpKind::Mul => {
                        panic!("cannot multiply `{}` by `{}`", ltype, rtype);
                    }
                    BinaryOpKind::Div => {
                        panic!("cannot divide `{}` by `{}`", ltype, rtype);
                    }
                    BinaryOpKind::Rem => {
                        panic!("cannot mod `{}` by `{}`", ltype, rtype);
                    }

                    BinaryOpKind::Eq => {
                        println!("\tceq");
                        return Type::Bool;
                    }
                    BinaryOpKind::Lt => {
                        println!("\tclt");
                        return Type::Bool;
                    }
                    BinaryOpKind::Le => {
                        println!("\tcgt");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                    BinaryOpKind::Ne => {
                        println!("\tceq");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                    BinaryOpKind::Gt => {
                        println!("\tcgt");
                        return Type::Bool;
                    }
                    BinaryOpKind::Ge => {
                        println!("\tclt");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                    _ => panic!("no implementation for `{}` {} `{}`", ltype, kind, rtype)
                }
                Type::String => match kind {
                    BinaryOpKind::Add => {
                        println!("\tcall string System.String::Concat(string, string)");
                    }
                    BinaryOpKind::Sub => {
                        panic!("cannot subtract `{}` from `{}`", ltype, rtype);
                    }
                    BinaryOpKind::Mul => {
                        panic!("cannot multiply `{}` by `{}`", ltype, rtype);
                    }
                    BinaryOpKind::Div => {
                        panic!("cannot divide `{}` by `{}`", ltype, rtype);
                    }
                    BinaryOpKind::Rem => {
                        panic!("cannot mod `{}` by `{}`", ltype, rtype);
                    }

                    BinaryOpKind::Eq => {
                        println!("\tcall bool System.String::op_Equality(string, string)");
                        return Type::Bool;
                    }
                    BinaryOpKind::Lt => {
                        println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                        println!("\tldc.i4.0");
                        println!("\tclt");
                        return Type::Bool;
                    }
                    BinaryOpKind::Le => {
                        println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                        println!("\tldc.i4.0");
                        println!("\tcgt");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                    BinaryOpKind::Ne => {
                        println!("call bool System.String::op_Inequality(string, string)");
                        return Type::Bool;
                    }
                    BinaryOpKind::Gt => {
                        println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                        println!("\tldc.i4.0");
                        println!("\tcgt");
                        return Type::Bool;
                    }
                    BinaryOpKind::Ge => {
                        println!("\tcallvirt instance int32 System.String::CompareTo(string)");
                        println!("\tldc.i4.0");
                        println!("\tclt");
                        println!("\tldc.i4.0");
                        println!("\tceq");
                        return Type::Bool;
                    }
                    _ => panic!("no implementation for `{}` {} `{}`", ltype, kind, rtype)
                }
                _ => panic!("no implementation for `{}` {} `{}`", ltype, kind, rtype)
            }
            match (&ltype, &rtype) {
                (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => rtype,
                (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => ltype,
                _ if ltype == rtype => ltype,
                _ => panic!("expected `{}`, found `{}`", ltype, rtype)
            }
        }
        NodeKind::ShortCircuitOp { kind, lhs, rhs } => {
            let end_label  = format!("IL_end{}", seq());
            match kind {
                ShortCircuitOpKind::And => {
                    println!("\tldc.i4.0");
                    let ltype = gen_il(*lhs, p);
                    if ltype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool, ltype);
                    }
                    println!("\tbrfalse {}", end_label);
                    println!("\tpop");
                    let rtype = gen_il(*rhs, p);
                    if rtype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool, rtype);
                    }
                    println!("{}:", end_label);
                }
                ShortCircuitOpKind::Or => {
                    println!("\tldc.i4.1");
                    let ltype = gen_il(*lhs, p);
                    if ltype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool, ltype);
                    }
                    println!("\tbrtrue {}", end_label);
                    println!("\tpop");
                    let rtype = gen_il(*rhs, p);
                    if rtype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool, rtype);
                    }
                    println!("{}:", end_label);
                }
            }
            Type::Bool
        }
        NodeKind::Semi { expr } => {
            let ty = gen_il(*expr, p);
            if ty != Type::Void {
                println!("\tpop");
            }
            ty
        }
        NodeKind::Path { segment, child } => {
            // TODO
            match child.kind {
                NodeKind::Path { .. } => gen_il(*child, p),
                NodeKind::Call { name, args } => {
                    if let Some(im) = p.find_impl(&segment) {
                        let func = im
                            .functions
                            .iter()
                            .find(|f|f.name==name)
                            .unwrap_or_else(|| panic!("The name '{}' does not exist in the current context", name));
                        for arg in args {
                            gen_il(arg, p);
                        }
                        let args = func
                            .param_symbol_table
                            .objs
                            .iter()
                            .skip(if func.is_static { 0 } else { 1 })
                            .map(|o|o.ty.to_ilstr())
                            .collect::<Vec<String>>()
                            .join(", ");
                        println!("\tcall {} {}::{}({})", func.rettype.to_ilstr(), segment, name, args);
                        func.rettype.clone()
                    } else {
                        panic!("The name '{}' does not exist in the current context", name);
                    }
                }
                _ => {
                    gen_il(*child, p)
                }
            }
        }
        NodeKind::Empty => {
            Type::Void
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
