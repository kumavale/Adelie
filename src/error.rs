use super::ast::*;
use super::keyword::*;
use super::token::*;
use super::utils::*;

/// any message
pub fn e0000(
    (path, lines, token): (&str, &[&str], &[Token]),
    message: &str,
) -> ! {
    disp_error_without_code();
    eprintln!("{}", message);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected ...
pub fn e0001((path, lines, token): (&str, &[&str], &[Token]), expect: TokenKind) -> ! {
    disp_error_with_code(1);
    eprintln!("expected `{}`, but got `{}`", expect, token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected type
pub fn e0002((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_with_code(2);
    eprintln!("expected type, but got `{}`", token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected identifier
pub fn e0003((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_with_code(3);
    eprintln!("expected identifier, but got `{}`", token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected item
pub fn e0004((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_with_code(4);
    eprintln!("expected item, but got `{}`", token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// tha name is defined multiple times
pub fn e0005((path, lines, token): (&str, &[&str], &[Token]), ident: &str) -> ! {
    disp_error_with_code(5);
    eprintln!("the name `{}` is defined multiple times", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// unknown start of token
pub fn e0006((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_with_code(6);
    eprintln!("unknown start of token `{}`", token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot find value in this scope
pub fn e0007((path, lines, token): (&str, &[&str], &[Token]), ident: &str) -> ! {
    disp_error_with_code(7);
    eprintln!("cannot find value `{}` in this scope", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected `,`, or `}`
pub fn e0008((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_with_code(8);
    eprintln!("expected `,`, or `}}`, found `{}`", token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`
pub fn e0009((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_with_code(9);
    eprintln!("expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`, found `{}`", token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `)`, `,`, `.`, `?`, or an operator
pub fn e0010((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_with_code(10);
    eprintln!("expected one of `)`, `,`, `.`, `?`, or an operator, found `{}`", token[0].kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expect `{type}`, found `{type}`
pub fn e0012(
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: &Type,
    actual: &Type,
) -> ! {
    disp_error_with_code(12);
    eprintln!("expect `{}`, found `{}`", expect, actual);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot find function `{}` in this scope
pub fn e0013(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
) -> ! {
    disp_error_with_code(13);
    eprintln!("cannot find function `{}` in this scope", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// no method named `{}` found for struct `{}` in the current scope
pub fn e0014(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
    method: &str,
) -> ! {
    disp_error_with_code(14);
    eprintln!("no method named `{}` found for struct `{}` in the current scope", method, stname);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// no field `{}` on type `{}`
pub fn e0015(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
    field: &str,
) -> ! {
    disp_error_with_code(15);
    eprintln!("no field `{}` on type `{}`", field, stname);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot find struct, variant or union type `{}` in this scope
pub fn e0016(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
) -> ! {
    disp_error_with_code(16);
    eprintln!("cannot find struct, variant or union type `{}` in this scope", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// missing fields in initializer of `{}`
pub fn e0017(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
) -> ! {
    disp_error_with_code(17);
    eprintln!("missing fields in initializer of `{}`", stname);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// `if` may be missing an `else` clause
pub fn e0018(
    (path, lines, token): (&str, &[&str], &[Token]),
    then_type: &Type,
) -> ! {
    disp_error_with_code(18);
    eprintln!("`if` may be missing an `else` clause");
    eprintln!("expect `()`, found `{}`", then_type);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// invalid left-hand side of assignment
pub fn e0019(
    (path, lines, token): (&str, &[&str], &[Token]),
) -> ! {
    disp_error_with_code(19);
    eprintln!("invalid left-hand side of assignment");
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot cast as `{}`
pub fn e0020(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    disp_error_with_code(20);
    eprintln!("cannot cast as `{}`", ty);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot apply unary operator `-` to type `{}`
pub fn e0021(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    disp_error_with_code(21);
    eprintln!("cannot apply unary operator `-` to type `{}`", ty);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// type `{}` cannot be dereferenced
pub fn e0022(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    disp_error_with_code(22);
    eprintln!("type `{}` cannot be dereferenced", ty);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot [+-*/%] `{}` to `{}`
pub fn e0023(
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
) -> ! {
    disp_error_with_code(23);
    match kind {
        BinaryOpKind::Add => eprintln!("cannot add `{}` to `{}`", lty, rty),
        BinaryOpKind::Sub => eprintln!("cannot subtract `{}` from `{}`", lty, rty),
        BinaryOpKind::Mul => eprintln!("cannot multiply `{}` by `{}`", lty, rty),
        BinaryOpKind::Div => eprintln!("cannot divide `{}` by `{}`", lty, rty),
        BinaryOpKind::Rem => eprintln!("cannot mod `{}` by `{}`", lty, rty),
        _ => unreachable!(),
    }
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// no implementation for `{}` {op} `{}`
pub fn e0024(
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
) -> ! {
    disp_error_with_code(24);
    eprintln!("no implementation for `{}` {} `{}`", lty, kind, rty);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

fn eprint_nearby(path: &str, lines: &[&str], token: &[Token]) -> Result<(), ()> {
    let begin  = &token[0];
    let end    = &token[token.len()-1];
    let digits = usize::digits(end.line);

    eprintln!("\x1b[34m{1:>0$}-->\x1b[0m {2}:{3}:{4}", digits, "", path, begin.line, begin.cur);
    if begin.line == end.line {
        let target_len = end.cur - begin.cur + begin.kind.to_string().len();
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {2}", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(target_len));
    } else if end.line - begin.line < 10 {
        eprintln!("[TODO: multiple line error message]");
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {2}", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            eprintln!("\x1b[34m{1:>0$} |\x1b[0m {2}", digits, line, lines.get(line-1).ok_or(())?);
        }
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            end.cur - end.kind.to_string().len(),
            "^".repeat(end.kind.to_string().len()));
    } else {
        eprintln!("[TODO: multiple line split error message]");
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {2}", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            eprintln!("\x1b[34m{1:>0$} |\x1b[0m {2}", digits, line, lines.get(line-1).ok_or(())?);
        }
        eprintln!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            end.cur - end.kind.to_string().len(),
            "^".repeat(end.kind.to_string().len()));
    }

    Ok(())
}

fn disp_error_with_code(code: usize) {
    eprint!("\x1b[31merror[E{:04}]: \x1b[0m", code);
}

fn disp_error_without_code() {
    eprint!("\x1b[31merror: \x1b[0m");
}
