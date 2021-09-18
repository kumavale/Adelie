use super::token::*;
use super::utils::*;

pub fn e0001<'a>(lines: std::str::Lines<'a>, token: &Token, expect: TokenKind) -> ! {
    eprintln!("expected `{:?}`, but got `{:?}`", expect, token.kind);
    eprint_nearby(lines, token.line).ok();
    panic!();
}

pub fn e0002<'a>(lines: std::str::Lines<'a>, token: &Token) -> ! {
    eprintln!("expected type, but got `{:?}`", token.kind);
    eprint_nearby(lines, token.line).ok();
    panic!();
}

pub fn e0003<'a>(lines: std::str::Lines<'a>, token: &Token) -> ! {
    eprintln!("expected identifier, but got `{:?}`", token.kind);
    eprint_nearby(lines, token.line).ok();
    panic!();
}

pub fn e0004<'a>(lines: std::str::Lines<'a>, token: &Token) -> ! {
    eprintln!("expected item, but got `{:?}`", token.kind);
    eprint_nearby(lines, token.line).ok();
    panic!();
}

pub fn e0005<'a>(lines: std::str::Lines<'a>, token: &Token, ident: &str) -> ! {
    eprintln!("the name `{}` is defined multiple times", ident);
    eprint_nearby(lines, token.line).ok();
    panic!();
}

pub fn e0006<'a>(lines: std::str::Lines<'a>, token: &Token) -> ! {
    eprintln!("unknown start of token `{:?}`", token.kind);
    eprint_nearby(lines, token.line).ok();
    panic!();
}

fn eprint_nearby<'a>(mut lines: std::str::Lines<'a>, line: usize) -> Result<(), ()> {
    let digits = usize::digits(line+1);

    if line == 1 {
        eprintln!("{1:>0$} | {2}", digits, line,   lines.nth(0).ok_or(())?);
        eprintln!("{1:>0$} | {2}", digits, line+1, lines.next().ok_or(())?);
    } else {
        eprintln!("{1:>0$} | {2}", digits, line-1, lines.nth(line-2).ok_or(())?);
        eprintln!("{1:>0$} | {2}", digits, line,   lines.next().ok_or(())?);
        eprintln!("{1:>0$} | {2}", digits, line+1, lines.next().ok_or(())?);
    }

    Ok(())
}
