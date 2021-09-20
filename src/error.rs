use std::str::Lines;
use super::token::*;
use super::utils::*;

/// expected ...
pub fn e0001((path, lines, token): (&str, Lines, &Token), expect: TokenKind) -> ! {
    eprintln!("expected `{:?}`, but got `{:?}`", expect, token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected type
pub fn e0002((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("expected type, but got `{:?}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected identifier
pub fn e0003((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("expected identifier, but got `{:?}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected item
pub fn e0004((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("expected item, but got `{:?}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// tha name is defined multiple times
pub fn e0005((path, lines, token): (&str, Lines, &Token), ident: &str) -> ! {
    eprintln!("the name `{}` is defined multiple times", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// unknown start of token
pub fn e0006((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("unknown start of token `{:?}`", token.kind);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// cannot find value in this scope
pub fn e0007((path, lines, token): (&str, Lines, &Token), ident: &str) -> ! {
    eprintln!("cannot find value `{}` in this scope", ident);
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected `,`, or `}`
pub fn e0008((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("expected `,`, or `}}`, found `{}`", token.kind.to_string());
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`
pub fn e0009((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`, found `{}`", token.kind.to_string());
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `)`, `,`, `.`, `?`, or an operator
pub fn e0010((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("expected one of `)`, `,`, `.`, `?`, or an operator, found `{}`", token.kind.to_string());
    eprint_nearby(path, lines, token).ok();
    panic!();
}

/// `break` outside of a loop
pub fn e0011((path, lines, token): (&str, Lines, &Token)) -> ! {
    eprintln!("cannot `break` outside of a loop");
    eprint_nearby(path, lines, token).ok();
    panic!();
}

fn eprint_nearby(path: &str, mut lines: Lines, token: &Token) -> Result<(), ()> {
    let line = token.line;
    let digits = usize::digits(line+1);
    let token_len = token.kind.to_string().len();

    eprintln!("{1:>0$}--> {2}:{3}:{4}", digits, "", path, token.line, token.cur);
    if line > 1 {
        eprintln!("{1:>0$} | {2}", digits, line-1, lines.nth(line-2).ok_or(())?);
    }
    eprintln!("{1:>0$} | {2}",          digits, line,   lines.next().ok_or(())?);
    eprintln!("{1:>0$} | {1:2$}{3:4$}", digits, "", token.cur - token_len, "^", token_len);
    eprintln!("{1:>0$} | {2}",          digits, line+1, lines.next().ok_or(())?);

    Ok(())
}
