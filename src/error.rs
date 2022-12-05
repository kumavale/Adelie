use crate::ast::*;
use crate::keyword::Type;
use crate::token::{Token, TokenKind};
use crate::utils::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct AdError {
    code: Option<usize>,
    message: String,
    near_by: String,
    stack_trace: String,
}

impl AdError {
    pub fn new(code: Option<usize>, message: String, near_by: String) -> Self {
        Self {
            code,
            message,
            near_by,
            stack_trace: std::backtrace::Backtrace::force_capture()
                .to_string()
                .lines()
                .take(6)
                .skip(4)
                .into_iter()
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }
}

#[derive(Debug)]
pub struct Errors {
    errors: Vec<AdError>,
}

impl Errors {
    pub fn new() -> Self {
        Self {
            errors: vec![],
        }
    }

    pub fn push(&mut self, aderr: AdError) {
        self.errors.push(aderr);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn err_count(&self) -> usize {
        self.errors.len()
    }

    pub fn display(&self) {
        for aderr in &self.errors {
            eprintln!("{}", message_with_error_code(aderr.code, &aderr.message));
            eprintln!("{}", aderr.near_by);
            #[cfg(debug_assertions)]
            eprintln!("{}", aderr.stack_trace);
            eprintln!();
        }
    }
}

macro_rules! disp_error_code {
    ($msg:expr) => (
        format!("\x1b[31merror: \x1b[0m{}", $msg)
    );
    ($code:expr, $msg:expr) => (
        format!("\x1b[31merror[E{:04}]: \x1b[0m{}", $code, $msg)
    );
}

fn message_with_error_code(code: Option<usize>, msg: &str) -> String {
    if let Some(code) = code {
        format!("\x1b[31merror[E{:04}]\x1b[0m: {}", code, msg)
    } else {
        format!("\x1b[31merror\x1b[0m: {}", msg)
    }
}

/// any message
pub fn e0000(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    message: &str,
){
    let aderr = AdError::new(None, message.to_string(), nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected `{}`, but got `{}`
pub fn e0001(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: TokenKind,
){
    let message = format!("expected `{}`, but got `{}`", expect, token[0].kind);
    let aderr = AdError::new(Some(1), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected type, but got `{}`
pub fn e0002(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected type, but got `{}`", token[0].kind);
    let aderr = AdError::new(Some(2), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected identifier, but got `{}`
pub fn e0003(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected identifier, but got `{}`", token[0].kind);
    let aderr = AdError::new(Some(3), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected item, but got `{}`
pub fn e0004(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected item, but got `{}`", token[0].kind);
    let aderr = AdError::new(Some(4), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// tha name is defined multiple times
pub fn e0005((path, lines, token): (&str, &[&str], &[Token]), ident: &str) -> ! {
    disp_error_code!(5);
    eprintln!("the name `{}` is defined multiple times", ident);
    nearby(path, lines, token).ok();
    panic!();
}

/// unknown start of token
pub fn e0006((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_code!(6);
    eprintln!("unknown start of token `{}`", token[0].kind);
    nearby(path, lines, token).ok();
    panic!();
}

/// cannot find value in this scope
pub fn e0007((path, lines, token): (&str, &[&str], &[Token]), ident: &str) -> ! {
    disp_error_code!(7);
    eprintln!("cannot find value `{}` in this scope", ident);
    nearby(path, lines, token).ok();
    panic!();
}

/// expected `,`, or `}`
pub fn e0008((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_code!(8);
    eprintln!("expected `,`, or `}}`, found `{}`", token[0].kind);
    nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`
pub fn e0009((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_code!(9);
    eprintln!("expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`, found `{}`", token[0].kind);
    nearby(path, lines, token).ok();
    panic!();
}

/// expected one of `)`, `,`, `.`, `?`, or an operator
pub fn e0010((path, lines, token): (&str, &[&str], &[Token])) -> ! {
    disp_error_code!(10);
    eprintln!("expected one of `)`, `,`, `.`, `?`, or an operator, found `{}`", token[0].kind);
    nearby(path, lines, token).ok();
    panic!();
}

/// expect `{type}`, found `{type}`
pub fn e0012(
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: &Type,
    actual: &Type,
) -> ! {
    disp_error_code!(12);
    eprintln!("expect `{}`, found `{}`", expect, actual);
    nearby(path, lines, token).ok();
    panic!();
}

/// cannot find function `{}` in this scope
pub fn e0013(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
) -> ! {
    disp_error_code!(13);
    eprintln!("cannot find function `{}` in this scope", ident);
    nearby(path, lines, token).ok();
    panic!();
}

/// no method named `{}` found for struct `{}` in the current scope
pub fn e0014(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
    method: &str,
) -> ! {
    disp_error_code!(14);
    eprintln!("no method named `{}` found for struct `{}` in the current scope", method, stname);
    nearby(path, lines, token).ok();
    panic!();
}

/// no field `{}` on type `{}`
pub fn e0015(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
    field: &str,
) -> ! {
    disp_error_code!(15);
    eprintln!("no field `{}` on type `{}`", field, stname);
    nearby(path, lines, token).ok();
    panic!();
}

/// cannot find struct, variant or union type `{}` in this scope
pub fn e0016(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
) -> ! {
    disp_error_code!(16);
    eprintln!("cannot find struct, variant or union type `{}` in this scope", ident);
    nearby(path, lines, token).ok();
    panic!();
}

/// missing fields in initializer of `{}`
pub fn e0017(
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
) -> ! {
    disp_error_code!(17);
    eprintln!("missing fields in initializer of `{}`", stname);
    nearby(path, lines, token).ok();
    panic!();
}

/// `if` may be missing an `else` clause
pub fn e0018(
    (path, lines, token): (&str, &[&str], &[Token]),
    then_type: &Type,
) -> ! {
    disp_error_code!(18);
    eprintln!("`if` may be missing an `else` clause");
    eprintln!("expect `()`, found `{}`", then_type);
    nearby(path, lines, token).ok();
    panic!();
}

/// invalid left-hand side of assignment
pub fn e0019(
    (path, lines, token): (&str, &[&str], &[Token]),
) -> ! {
    disp_error_code!(19);
    eprintln!("invalid left-hand side of assignment");
    nearby(path, lines, token).ok();
    panic!();
}

/// cannot cast as `{}`
pub fn e0020(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    disp_error_code!(20);
    eprintln!("cannot cast as `{}`", ty);
    nearby(path, lines, token).ok();
    panic!();
}

/// cannot apply unary operator `-` to type `{}`
pub fn e0021(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    disp_error_code!(21);
    eprintln!("cannot apply unary operator `-` to type `{}`", ty);
    nearby(path, lines, token).ok();
    panic!();
}

/// type `{}` cannot be dereferenced
pub fn e0022(
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
) -> ! {
    disp_error_code!(22);
    eprintln!("type `{}` cannot be dereferenced", ty);
    nearby(path, lines, token).ok();
    panic!();
}

/// cannot [+-*/%] `{}` to `{}`
pub fn e0023(
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
) -> ! {
    disp_error_code!(23);
    match kind {
        BinaryOpKind::Add => eprintln!("cannot add `{}` to `{}`", lty, rty),
        BinaryOpKind::Sub => eprintln!("cannot subtract `{}` from `{}`", lty, rty),
        BinaryOpKind::Mul => eprintln!("cannot multiply `{}` by `{}`", lty, rty),
        BinaryOpKind::Div => eprintln!("cannot divide `{}` by `{}`", lty, rty),
        BinaryOpKind::Rem => eprintln!("cannot mod `{}` by `{}`", lty, rty),
        _ => unreachable!(),
    }
    nearby(path, lines, token).ok();
    panic!();
}

/// no implementation for `{}` {op} `{}`
pub fn e0024(
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
) -> ! {
    disp_error_code!(24);
    eprintln!("no implementation for `{}` {} `{}`", lty, kind, rty);
    nearby(path, lines, token).ok();
    panic!();
}

/// use of possibly-uninitialized variable: `{}`
pub fn e0027(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident:  &str,
) -> ! {
    disp_error_code!(27);
    eprintln!("use of possibly-uninitialized variable: `{}`", ident);
    nearby(path, lines, token).ok();
    panic!();
}

/// cannot assign twice to immutable variable: `{}`
pub fn e0028(
    (path, lines, token): (&str, &[&str], &[Token]),
    ident:  &str,
) -> ! {
    disp_error_code!(28);
    eprintln!("cannot assign twice to immutable variable: `{}`", ident);
    nearby(path, lines, token).ok();
    panic!();
}

/// this function takes {} argument[s] but {} argument[s] were supplied
pub fn e0029(
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: usize,
    actual: usize,
) -> ! {
    disp_error_code!(29);
    eprintln!("this function takes {expect} argument but {actual} argument were supplied");
    nearby(path, lines, token).ok();
    panic!();
}

fn nearby(path: &str, lines: &[&str], token: &[Token]) -> Result<String, ()> {
    let begin  = &token[0];
    let end    = &token[token.len()-1];
    let digits = usize::digits(end.line);
    let mut message = String::new();

    message += &format!("\x1b[34m{1:>0$}-->\x1b[0m {2}:{3}:{4}\n", digits, "", path, begin.line, begin.cur);
    if begin.line == end.line {
        let target_len = end.cur - begin.cur + begin.kind.to_string().len();
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(target_len));
    } else if end.line - begin.line < 10 {
        message += "[TODO: multiple line error message]\n";
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m\n",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, line, lines.get(line-1).ok_or(())?);
        }
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            end.cur - end.kind.to_string().len(),
            "^".repeat(end.kind.to_string().len()));
    } else {
        message += "[TODO: multiple line split error message]\n";
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m\n",
            digits, "",
            begin.cur - begin.kind.to_string().len(),
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, line, lines.get(line-1).ok_or(())?);
        }
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            end.cur - end.kind.to_string().len(),
            "^".repeat(end.kind.to_string().len()));
    }

    Ok(message)
}
