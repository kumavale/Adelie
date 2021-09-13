use super::ast::*;
use super::builtin::*;
use super::keyword::*;
use super::program::*;

pub fn gen_il(node: Node, p: &Program) -> Type {
    match node {
        Node::Integer { typekind, num } => {
            println!("\tldc.i4 {}", num as i32);
            typekind
        }
        Node::String { typekind, str } => {
            println!("\tldstr \"{}\"", str);
            typekind
        }
        Node::Builtin { kind, args } => {
            gen_builtin_il(kind, args, p)
        }
        Node::Call { name, args } => {
            for arg in args {
                gen_il(arg, p);
            }
            if let Some(func) = p.find_function(&name) {
                let args = func.param_symbol_table.objs.iter().map(|o|o.typekind.to_ilstr()).collect::<Vec<String>>().join(", ");
                println!("\tcall {} {}({})", func.rettype.to_ilstr(), name, args);
                func.rettype.clone()
            } else {
                panic!("The name '{}' does not exist in the current context", name);
            }
        }
        Node::Struct { obj, field } => {
            if let Some(st) = p.find_struct(&obj.typekind.to_str()) {
                if field.len() != st.field.len() {
                    panic!("missing field");
                }
                println!("\tldloca {}", obj.offset);
                println!("\tinitobj {}", obj.typekind.to_str());
                for value in field.into_iter().zip(&st.field) {
                    println!("\tldloca {}", obj.offset);
                    gen_il(value.0, p);
                    println!("\tstfld {} {}::{}", value.1.typekind.to_ilstr(), obj.typekind.to_str(), value.1.name);
                }
                println!("\tldloc {}", obj.offset);
            } else {
                panic!("The name '{}' does not exist in the current context", obj.name);
            }
            Type::Struct(obj.name.to_string())
        }
        Node::Field { name, expr } => {
            let typekind = gen_il(*expr, p);
            // TODO
            typekind
        }
        Node::Variable { obj } => {
            if obj.is_param {
                println!("\tldarg {}", obj.offset);
            } else {
                println!("\tldloc {}", obj.offset);
            }
            obj.typekind.clone()
        }
        Node::Block { stmts } => {
            let mut typekind = Type::Void;
            for stmt in stmts {
                if let Node::Evaluates { expr: _ } = stmt {
                    typekind = gen_il(stmt, p);
                } else {
                    typekind = gen_il(stmt, p);
                    if typekind != Type::Void {
                        println!("\tpop");
                    }
                }
            }
            typekind
        }
        Node::If { cond, then, els } => {
            let cond_type = gen_il(*cond, p);
            if cond_type != Type::Bool {
                panic!("expected `{}`, found `{}`", Type::Bool.to_str(), cond_type.to_str());
            }
            let else_label = format!("IL_else{}", seq());
            let end_label = format!("IL_end{}", seq());
            println!("\tbrfalse {}", else_label);
            let then_type = gen_il(*then, p);
            println!("\tbr {}", end_label);
            println!("{}:", else_label);
            let els_type = els.map(|els| gen_il(*els, p));
            println!("{}:", end_label);
            if let Some(els_type) = els_type {
                if els_type != then_type {
                    panic!("expected `{}`, found `{}`", then_type.to_ilstr(), els_type.to_ilstr())
                }
            }
            then_type
        }
        Node::While { cond, then } => {
            let begin_label = format!("IL_begin{}", seq());
            let end_label = format!("IL_end{}", seq());
            println!("{}:", begin_label);
            let cond_type = gen_il(*cond, p);
            if cond_type != Type::Bool {
                panic!("expected `{}`, found `{}`", Type::Bool.to_str(), cond_type.to_str());
            }
            println!("\tbrfalse {}", end_label);
            let then_type = gen_il(*then, p);
            println!("\tbr {}", begin_label);
            println!("{}:", end_label);
            then_type
        }
        Node::Loop { then } => {
            let begin_label = format!("IL_begin{}", seq());
            let end_label = format!("IL_end{}", seq());
            println!("{}:", begin_label);
            let then_type = gen_il(*then, p);
            println!("\tbr {}", begin_label);
            println!("{}:", end_label);
            then_type
        }
        Node::Assign { lhs, rhs } => {
            match *lhs {
                Node::Variable { obj } => {
                    gen_il(*rhs, p);
                    println!("\tstloc {}", obj.offset);
                }
                Node::UnaryOp { kind: UnaryOpKind::Deref, expr } => {
                    //match expr {
                    //    Node::Variable { obj }
                    //}
                    gen_il(*expr, p);
                    gen_il(*rhs, p);
                    println!("\tstind.i4");
                }
                _ => panic!("The left-hand side of an assignment must be a variable")
            }
            Type::Void
        }
        Node::Return { expr } => {
            let rettype = if let Some(expr) = expr {
                gen_il(*expr, p)
            } else {
                Type::Void
            };
            println!("\tret");
            rettype
        }
        Node::Evaluates { expr } => {
            gen_il(*expr, p)
        }
        Node::Cast { typekind: new_type, expr } => {
            let old_type = gen_il(*expr, p);
            match new_type {
                Type::Numeric(Numeric::I32) => {
                    println!("\tconv.i4");
                }
                Type::Bool => {
                    if let Type::Numeric(_) = old_type {
                        panic!("invalid cast as `{}`", Type::Bool.to_str());
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
                t => panic!("invalid cast as `{}`", t.to_str()),
            }
            new_type
        }
        Node::UnaryOp { kind, expr } => {
            match kind {
                UnaryOpKind::Not => {
                    let typekind = gen_il(*expr, p);
                    match typekind {
                        Type::Bool => {
                            println!("\tldc.i4.0");
                            println!("\tceq");
                        }
                        _ => println!("\tnot")
                    }
                    typekind
                }
                UnaryOpKind::Neg => {
                    let typekind = gen_il(*expr, p);
                    println!("\tneg");
                    typekind
                }
                UnaryOpKind::Ref => {
                    if let Node::Variable { obj } = *expr {
                        if obj.is_param {
                            println!("\tldarga {}", obj.offset);
                        } else {
                            println!("\tldloca {}", obj.offset);
                        }
                        Type::Ptr(Box::new(obj.typekind.clone()))
                    } else {
                        Type::Ptr(Box::new(gen_il(*expr, p)))
                    }
                }
                UnaryOpKind::Deref => {
                    let typekind = gen_il(*expr, p);
                    if let Type::Ptr(typekind) = typekind {
                        match *typekind {
                            Type::Ptr(_) => println!("\tldind.i"),
                            Type::Numeric(Numeric::I32) => println!("\tldind.i4"),
                            _ => unimplemented!(),
                        }
                        *typekind
                    } else {
                        panic!("type `{}` cannot be dereferenced", typekind.to_str());
                    }
                }
            }
        }
        Node::BinaryOp { kind, lhs, rhs } => {
            let ltype = gen_il(*lhs, p);
            let rtype = gen_il(*rhs, p);
            if ltype != rtype {
                panic!("expected `{}`, found `{}`", ltype.to_str(), rtype.to_str());
            }
            match kind {
                BinaryOpKind::Add => { println!("\tadd"); ltype }
                BinaryOpKind::Sub => { println!("\tsub"); ltype }
                BinaryOpKind::Mul => { println!("\tmul"); ltype }
                BinaryOpKind::Div => { println!("\tdiv"); ltype }
                BinaryOpKind::Rem => { println!("\trem"); ltype }

                BinaryOpKind::BitXor => { println!("\txor"); ltype }
                BinaryOpKind::BitAnd => { println!("\tand"); ltype }
                BinaryOpKind::BitOr  => { println!("\tor");  ltype }

                BinaryOpKind::Shl => { println!("\tshl"); ltype }
                BinaryOpKind::Shr => { println!("\tshr"); ltype }

                BinaryOpKind::Eq => { println!("\tceq"); Type::Bool }
                BinaryOpKind::Lt => { println!("\tclt"); Type::Bool }
                BinaryOpKind::Le => {
                    println!("\tcgt");
                    println!("\tldc.i4.0");
                    println!("\tceq");
                    Type::Bool
                }
                BinaryOpKind::Ne => {
                    println!("\tceq");
                    println!("\tldc.i4.0");
                    println!("\tceq");
                    Type::Bool
                }
                BinaryOpKind::Gt => { println!("\tcgt"); Type::Bool }
                BinaryOpKind::Ge => {
                    println!("\tclt");
                    println!("\tldc.i4.0");
                    println!("\tceq");
                    Type::Bool
                }
            }
        }
        Node::ShortCircuitOp { kind, lhs, rhs } => {
            let end_label  = format!("IL_end{}", seq());
            match kind {
                ShortCircuitOpKind::And => {
                    println!("\tldc.i4.0");
                    let ltype = gen_il(*lhs, p);
                    if ltype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool.to_str(), ltype.to_str());
                    }
                    println!("\tbrfalse {}", end_label);
                    println!("\tpop");
                    let rtype = gen_il(*rhs, p);
                    if rtype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool.to_str(), rtype.to_str());
                    }
                    println!("{}:", end_label);
                }
                ShortCircuitOpKind::Or => {
                    println!("\tldc.i4.1");
                    let ltype = gen_il(*lhs, p);
                    if ltype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool.to_str(), ltype.to_str());
                    }
                    println!("\tbrtrue {}", end_label);
                    println!("\tpop");
                    let rtype = gen_il(*rhs, p);
                    if rtype != Type::Bool {
                        panic!("expected `{}`, found `{}`", Type::Bool.to_str(), rtype.to_str());
                    }
                    println!("{}:", end_label);
                }
            }
            Type::Bool
        }
    }
}

fn seq() -> usize {
    unsafe {
        static mut SEQ: usize = 0;
        SEQ += 1;
        SEQ
    }
}
