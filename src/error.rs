use super::ast::*;
use super::keyword::*;
use super::token::*;
use super::utils::*;

/// expected ...
pub fn e0001((path, lines, token): (&str, &[&str], &Token), expect: TokenKind) -> ! {
    eprintln!("error[E0001]: expected `{}`, but got `{}`", expect, token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected type
pub fn e0002((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0002]: expected type, but got `{}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected identifier
pub fn e0003((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0003]: expected identifier, but got `{}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected item
pub fn e0004((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0004]: expected item, but got `{}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// tha name is defined multiple times
pub fn e0005((path, lines, token): (&str, &[&str], &Token), ident: &str) -> ! {
    eprintln!("error[E0005]: the name `{}` is defined multiple times", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// unknown start of token
pub fn e0006((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0006]: unknown start of token `{}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot find value in this scope
pub fn e0007((path, lines, token): (&str, &[&str], &Token), ident: &str) -> ! {
    eprintln!("error[E0007]: cannot find value `{}` in this scope", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected `,`, or `}`
pub fn e0008((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0008]: expected `,`, or `}}`, found `{}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`
pub fn e0009((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0009]: expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`, found `{}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `)`, `,`, `.`, `?`, or an operator
pub fn e0010((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0010]: expected one of `)`, `,`, `.`, `?`, or an operator, found `{}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// `break` outside of a loop
pub fn e0011((path, lines, token): (&str, &[&str], &Token)) -> ! {
    eprintln!("error[E0011]: cannot `break` outside of a loop");
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expect `{type}`, found `{type}`
pub fn e0012(
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: &Type,
    actual: &Type,
) -> ! {
    eprintln!("error[E0012]: expect `{}`, found `{}`", expect, actual);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot find function `{}` in this scope
pub fn e0013(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
) -> ! {
    eprintln!("error[E0013]: cannot find function `{}` in this scope", ident);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// no method named `{}` found for struct `{}` in the current scope
pub fn e0014(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
    method: &str,
) -> ! {
    eprintln!("error[E0014]: no method named `{}` found for struct `{}` in the current scope", method, stname);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// no field `{}` on type `{}`
pub fn e0015(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
    field: &str,
) -> ! {
    eprintln!("error[E0015]: no field `{}` on type `{}`", field, stname);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot find struct, variant or union type `{}` in this scope
pub fn e0016(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
) -> ! {
    eprintln!("error[E0016]: cannot find struct, variant or union type `{}` in this scope", ident);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// missing fields in initializer of `{}`
pub fn e0017(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
) -> ! {
    eprintln!("error[E0017]: missing fields in initializer of `{}`", stname);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// `if` may be missing an `else` clause
pub fn e0018(
    (path, lines, token): (&str, &[&str], &[Token]),
    then_type: &Type,
) -> ! {
    eprintln!("error[E0018]: `if` may be missing an `else` clause");
    eprintln!("expect `()`, found `{}`", then_type);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// invalid left-hand side of assignment
pub fn e0019(
    (path, lines, token): (&str, &[&str], &[Token]),
) -> ! {
    eprintln!("error[E0019]: invalid left-hand side of assignment");
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot cast as `{}`
pub fn e0020(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    eprintln!("error[E0020]: cannot cast as `{}`", ty);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot apply unary operator `-` to type `{}`
pub fn e0021(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    eprintln!("error[E0021]: cannot apply unary operator `-` to type `{}`", ty);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// type `{}` cannot be dereferenced
pub fn e0022(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    eprintln!("error[E0022]: type `{}` cannot be dereferenced", ty);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot [+-*/%] `{}` to `{}`
pub fn e0023(
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
) -> ! {
    eprint!("error[E0023]: ");
    match kind {
        BinaryOpKind::Add => eprintln!("cannot add `{}` to `{}`", lty, rty),
        BinaryOpKind::Sub => eprintln!("cannot subtract `{}` from `{}`", lty, rty),
        BinaryOpKind::Mul => eprintln!("cannot multiply `{}` by `{}`", lty, rty),
        BinaryOpKind::Div => eprintln!("cannot divide `{}` by `{}`", lty, rty),
        BinaryOpKind::Rem => eprintln!("cannot mod `{}` by `{}`", lty, rty),
        _ => unreachable!(),
    }
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

/// no implementation for `{}` {op} `{}`
pub fn e0024(
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
) -> ! {
    eprintln!("error[E0024]: no implementation for `{}` {} `{}`", lty, kind, rty);
    _eprint_nearby(path, lines, token).ok();
    panic!();
}

pub fn eprint_nearby(path: &str, lines: &[&str], token: &Token) -> Result<(), ()> {
    let line = token.line;
    let digits = usize::digits(line+1);
    let token_len = token.kind.to_string().len();

    eprintln!("{1:>0$}--> {2}:{3}:{4}", digits, "", path, token.line, token.cur);
    if line > 1 {
        eprintln!("{1:>0$} | {2}", digits, line-1, lines.get(line-2).ok_or(())?);
    }
    eprintln!("{1:>0$} | {2}",       digits, line,   lines.get(line-1).ok_or(())?);
    eprintln!("{1:>0$} | {1:2$}{3}", digits, "", token.cur - token_len, "^".repeat(token_len));
    eprintln!("{1:>0$} | {2}",       digits, line+1, lines.get(line).ok_or(())?);

    Ok(())
}

pub fn _eprint_nearby(path: &str, lines: &[&str], token: &[Token]) -> Result<(), ()> {
    let begin  = &token[0];
    let end    = &token[token.len()-1];
    let digits = usize::digits(end.line);

    eprintln!("{1:>0$}--> {2}:{3}:{4}", digits, "", path, begin.line, begin.cur);
    if begin.line == end.line {
        let target_len = end.cur - begin.cur + begin.kind.to_string().len();
        eprintln!("{1:>0$} | {2}", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        eprintln!("{1:>0$} | {1:2$}{3}",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(target_len));
    } else if end.line - begin.line < 10 {
        eprintln!("[TODO: multiple line error message]");
        eprintln!("{1:>0$} | {2}", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        eprintln!("{1:>0$} | {1:2$}{3}",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            eprintln!("{1:>0$} | {2}", digits, line, lines.get(line-1).ok_or(())?);
        }
        eprintln!("{1:>0$} | {1:2$}{3}",
            digits, "",
            end.cur - end.kind.to_string().len(),
            "^".repeat(end.kind.to_string().len()));
    } else {
        eprintln!("[TODO: multiple line split error message]");
        eprintln!("{1:>0$} | {2}", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        eprintln!("{1:>0$} | {1:2$}{3}",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            eprintln!("{1:>0$} | {2}", digits, line, lines.get(line-1).ok_or(())?);
        }
        eprintln!("{1:>0$} | {1:2$}{3}",
            digits, "",
            end.cur - end.kind.to_string().len(),
            "^".repeat(end.kind.to_string().len()));
    }

    Ok(())
}
