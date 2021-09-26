use super::ast::*;
use super::builtin::*;
use super::error::*;
use super::keyword::*;
use super::program::*;

pub fn gen_il(node: Node, p: &Program) -> Type {
    match node.kind {
        NodeKind::Integer { ty, num } => {
            use super::token::*;
            debug_assert!(matches!(node.token[0].kind,
                TokenKind::Literal(LiteralKind::Char(_))
              | TokenKind::Literal(LiteralKind::Integer(_))
              | TokenKind::Keyword(Keyword::True)
              | TokenKind::Keyword(Keyword::False)));
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
                    let token = arg.token;
                    let arg_ty = gen_il(arg, p);
                    match (&arg_ty, &param_ty) {
                        (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                        (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => unreachable!(),
                        _ if &arg_ty == *param_ty => (),
                        _ => e0012((p.path, &p.lines, token), &arg_ty, param_ty)
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
                e0013((p.path, &p.lines, node.token), &name);
            }
        }
        NodeKind::Method { expr, ident, args } => {
            match gen_il(*expr, p) {
                Type::Struct(st) => {
                    if let Some(im) = p.find_impl(&st) {
                        let func = if let Some(func) = im
                            .functions
                            .iter()
                            .find(|f|f.name==ident && !f.is_static)
                        {
                            func
                        } else {
                            e0014((p.path, &p.lines, node.token), &ident, &st);
                        };
                        let params = func
                            .param_symbol_table
                            .objs
                            .iter()
                            .skip(if func.is_static { 0 } else { 1 })
                            .map(|o|&o.ty)
                            .collect::<Vec<&Type>>();
                        for (arg, param_ty) in args.into_iter().zip(&params) {
                            let token = arg.token;
                            let arg_ty = gen_il(arg, p);
                            match (&arg_ty, &param_ty) {
                                (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => (),
                                (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => unreachable!(),
                                _ if &arg_ty == *param_ty => (),
                                _ => e0012((p.path, &p.lines, token), &arg_ty, param_ty)
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
                        e0014((p.path, &p.lines, node.token), &ident, &st);
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
                    e0017((p.path, &p.lines, node.token), &st.name);
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
                e0016((p.path, &p.lines, node.token), &obj.name);
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
                    e0015((p.path, &p.lines, node.token), &stname, &ident);
                }
            } else {
                e0016((p.path, &p.lines, node.token), &stname);
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
            let token = cond.token;
            let cond_type = gen_il(*cond, p);
            if cond_type != Type::Bool {
                e0012((p.path, &p.lines, token), &Type::Bool, &cond_type);
            }
            let seq = seq();
            let else_label = format!("IL_else{}", seq);
            let end_label = format!("IL_end{}", seq);
            println!("\tbrfalse {}", else_label);
            let then_type = gen_il(*then, p);
            println!("\tbr {}", end_label);
            println!("{}:", else_label);
            let els_type = els.map(|els| (els.token, gen_il(*els, p)));
            println!("{}:", end_label);
            if let Some(els_type) = els_type {
                match (&then_type, &els_type.1) {
                    (Type::Numeric(Numeric::Integer), Type::Numeric(..)) => els_type.1,
                    (Type::Numeric(..), Type::Numeric(Numeric::Integer)) => then_type,
                    _ if then_type == els_type.1 => then_type,
                    _ => e0012((p.path, &p.lines, node.token), &then_type, &els_type.1)
                }
            } else if then_type != Type::Void {
                e0018((p.path, &p.lines, node.token), &then_type);
            } else {
                then_type
            }
        }
        NodeKind::While { cond, then, brk_label_seq } => {
            let begin_label = format!("IL_begin{}", seq());
            let end_label = format!("IL_break{}", brk_label_seq);
            println!("{}:", begin_label);
            let token = cond.token;
            let cond_type = gen_il(*cond, p);
            if cond_type != Type::Bool {
                e0012((p.path, &p.lines, token), &Type::Bool, &cond_type);
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
                    if let Type::Struct(stname) = gen_il(*expr, p) {
                        if let Some(st) = p.find_struct(&stname) {
                            if let Some(field) = st.field.iter().find(|o|o.name==ident) {
                                gen_il(*rhs, p);
                                println!("\tstfld {} {}::{}", field.ty.to_ilstr(), stname, ident);
                            } else {
                                e0015((p.path, &p.lines, node.token), &stname, &ident);
                            }
                        } else {
                            e0016((p.path, &p.lines, node.token), &stname);
                        }
                    } else {
                        unimplemented!("primitive type");
                    }
                }
                _ => e0019((p.path, &p.lines, node.token))
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
                        e0020((p.path, &p.lines, node.token), &Type::Bool);
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
                t => e0020((p.path, &p.lines, node.token), &t)
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
                    if let Type::Numeric(..) = ty {
                        println!("\tneg");
                    } else {
                        e0021((p.path, &p.lines, node.token), &ty);
                    }
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
                        e0022((p.path, &p.lines, node.token), &ty);
                    }
                }
            }
        }
        NodeKind::BinaryOp { kind, lhs, rhs } => {
            let ltype = gen_il(*lhs, p);
            let rtype = gen_il(*rhs, p);
            let mut is_bool = false;
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
                        e0023((p.path, &p.lines, node.token),
                            kind, &ltype, &rtype);
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
                    _ => e0024((p.path, &p.lines, node.token), kind, &ltype, &rtype)
                }
                Type::String => match kind {
                    BinaryOpKind::Add => {
                        println!("\tcall string System.String::Concat(string, string)");
                    }
                    BinaryOpKind::Sub |
                    BinaryOpKind::Mul |
                    BinaryOpKind::Div |
                    BinaryOpKind::Rem => {
                        e0023((p.path, &p.lines, node.token),
                            kind, &ltype, &rtype);
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
                    _ => e0024((p.path, &p.lines, node.token), kind, &ltype, &rtype)
                }
                _ => e0024((p.path, &p.lines, node.token), kind, &ltype, &rtype)
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
                _ => e0012((p.path, &p.lines, node.token), &ltype, &rtype)
            }
        }
        NodeKind::ShortCircuitOp { kind, lhs, rhs } => {
            let end_label  = format!("IL_end{}", seq());
            match kind {
                ShortCircuitOpKind::And => {
                    println!("\tldc.i4.0");
                    let token = lhs.token;
                    let ltype = gen_il(*lhs, p);
                    if ltype != Type::Bool {
                        e0012((p.path, &p.lines, token), &Type::Bool, &ltype);
                    }
                    println!("\tbrfalse {}", end_label);
                    println!("\tpop");
                    let token = rhs.token;
                    let rtype = gen_il(*rhs, p);
                    if rtype != Type::Bool {
                        e0012((p.path, &p.lines, token), &Type::Bool, &rtype);
                    }
                    println!("{}:", end_label);
                }
                ShortCircuitOpKind::Or => {
                    println!("\tldc.i4.1");
                    let token = lhs.token;
                    let ltype = gen_il(*lhs, p);
                    if ltype != Type::Bool {
                        e0012((p.path, &p.lines, token), &Type::Bool, &ltype);
                    }
                    println!("\tbrtrue {}", end_label);
                    println!("\tpop");
                    let token = rhs.token;
                    let rtype = gen_il(*rhs, p);
                    if rtype != Type::Bool {
                        e0012((p.path, &p.lines, token), &Type::Bool, &rtype);
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
                        let func = if let Some(func) = im
                            .functions
                            .iter()
                            .find(|f|f.name==name) {
                                func
                            } else {
                                e0014((p.path, &p.lines, node.token), &segment, &name);
                            };
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
                        e0014((p.path, &p.lines, node.token), &segment, &name);
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
