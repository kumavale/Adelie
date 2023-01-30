use crate::ast::*;
use crate::keyword::Type;
use crate::token::{Token, TokenKind};
use crate::utils::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AdErrorKind {
    Warning,
    Deny,
}

#[derive(Debug)]
pub struct AdError {
    kind: AdErrorKind,
    code: Option<usize>,
    message: String,
    near_by: String,
    stack_trace: String,
}

impl AdError {
    pub fn new(kind: AdErrorKind, code: Option<usize>, message: String, near_by: String) -> Self {
        Self {
            kind,
            code,
            message,
            near_by,
            stack_trace: std::backtrace::Backtrace::force_capture()
                .to_string()
                .lines()
                .take(8)
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

    pub fn any_deny(&self) -> bool {
        self.errors.iter().any(|e| e.kind == AdErrorKind::Deny)
    }

    pub fn err_count(&self) -> usize {
        self.errors.iter().fold(0, |acc, x| if x.kind == AdErrorKind::Deny { acc + 1 } else { acc })
    }

    pub fn display(&self) {
        for aderr in &self.errors {
            eprintln!("{}", message_with_error_code(aderr.kind, aderr.code, &aderr.message));
            eprintln!("{}", aderr.near_by);
            #[cfg(debug_assertions)]
            eprintln!("{}", aderr.stack_trace);
            eprintln!();
        }
    }
}

fn message_with_error_code(kind: AdErrorKind, code: Option<usize>, msg: &str) -> String {
    let kindmsg = match kind {
        AdErrorKind::Warning => "\x1b[33mwarning",
        AdErrorKind::Deny    => "\x1b[31merror",
    };
    if let Some(code) = code {
        format!("{}[E{:04}]\x1b[0m: {}", kindmsg, code, msg)
    } else {
        format!("{}\x1b[0m: {}", kindmsg, msg)
    }
}

/// any message with warning
pub fn warning(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    message: &str,
){
    let aderr = AdError::new(AdErrorKind::Warning, None, message.to_string(), nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// any message with error
pub fn e0000(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    message: &str,
){
    let aderr = AdError::new(AdErrorKind::Deny, None, message.to_string(), nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected `{}`, but got `{}`
pub fn e0001(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: TokenKind,
){
    let message = format!("expected `{}`, but got `{}`", expect, token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(1), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected type, but got `{}`
pub fn e0002(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected type, but got `{}`", token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(2), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected identifier, but got `{}`
pub fn e0003(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected identifier, but got `{}`", token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(3), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected item, but got `{}`
pub fn e0004(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected item, but got `{}`", token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(4), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// tha name is defined multiple times
pub fn e0005(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
){
    let message = format!("the name `{}` is defined multiple times", ident);
    let aderr = AdError::new(AdErrorKind::Deny, Some(5), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// unknown start of token `{}`
pub fn e0006(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("unknown start of token `{}`", token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(6), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// cannot find value `{}` in this scope
pub fn e0007(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
){
    let message = format!("cannot find value `{}` in this scope", ident);
    let aderr = AdError::new(AdErrorKind::Deny, Some(7), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected `,`, or `}}`, found `{}`
pub fn e0008(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected `,`, or `}}`, found `{}`", token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(8), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`, found `{}`
pub fn e0009(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected one of `!`, `(`, `)`, `+`, `,`, `::`, or `<`, found `{}`", token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(9), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expected one of `)`, `,`, `.`, `?`, or an operator, found `{}`
pub fn e0010(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = format!("expected one of `)`, `,`, `.`, `?`, or an operator, found `{}`", token[0].kind);
    let aderr = AdError::new(AdErrorKind::Deny, Some(10), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// expect `{type}`, found `{type}`
pub fn e0012(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: &Type,
    actual: &Type,
){
    let message = if cfg!(debug_assertions) {
        format!("expect `{:?}`, found `{:?}`", expect, actual)
    } else {
        format!("expect `{}`, found `{}`", expect, actual)
    };
    let aderr = AdError::new(AdErrorKind::Deny, Some(12), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// cannot find function `{}` in this scope
pub fn e0013(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
){
    let message = format!("cannot find function `{}` in this scope", ident);
    let aderr = AdError::new(AdErrorKind::Deny, Some(13), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// no method named `{}` found for struct `{}` in the current scope
pub fn e0014(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    method: &str,
    stname: &str,
){
    let message = format!("no method named `{}` found for struct `{}` in the current scope", method, stname);
    let aderr = AdError::new(AdErrorKind::Deny, Some(14), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// no field `{}` on type `{}`
pub fn e0015(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    field: &str,
    stname: &str,
){
    let message = format!("no field `{}` on type `{}`", field, stname);
    let aderr = AdError::new(AdErrorKind::Deny, Some(15), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// cannot find struct, variant or union type `{}` in this scope
pub fn e0016(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ident: &str,
){
    let message = format!("cannot find struct, variant or union type `{}` in this scope", ident);
    let aderr = AdError::new(AdErrorKind::Deny, Some(16), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// missing fields in initializer of `{}`
pub fn e0017(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    stname: &str,
){
    let message = format!("missing fields in initializer of `{}`", stname);
    let aderr = AdError::new(AdErrorKind::Deny, Some(17), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// `if` may be missing an `else` clause
pub fn e0018(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    then_type: &Type,
){
    let mut message = "`if` may be missing an `else` clause\n".to_string();
    // ↓TODO: message details
    message += &format!("              expect `()`, found `{}`", then_type);
    let aderr = AdError::new(AdErrorKind::Deny, Some(18), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// invalid left-hand side of assignment
pub fn e0019(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
){
    let message = "invalid left-hand side of assignment".to_string();
    let aderr = AdError::new(AdErrorKind::Deny, Some(19), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// cannot cast as `{}`
pub fn e0020(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
){
    let message = format!("cannot cast as `{}`", ty);
    let aderr = AdError::new(AdErrorKind::Deny, Some(20), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// cannot apply unary operator `-` to type `{}`
pub fn e0021(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
){
    let message = format!("cannot apply unary operator `-` to type `{}`", ty);
    let aderr = AdError::new(AdErrorKind::Deny, Some(21), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// type `{}` cannot be dereferenced
pub fn e0022(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ty: &Type,
){
    let message = format!("type `{}` cannot be dereferenced", ty);
    let aderr = AdError::new(AdErrorKind::Deny, Some(22), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// cannot [+-*/%] `{}` to `{}`
pub fn e0023(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
){
    let message = match kind {
        BinaryOpKind::Add => format!("cannot add `{}` to `{}`",        rty, lty),
        BinaryOpKind::Sub => format!("cannot subtract `{}` from `{}`", rty, lty),
        BinaryOpKind::Mul => format!("cannot multiply `{}` by `{}`",   lty, rty),
        BinaryOpKind::Div => format!("cannot divide `{}` by `{}`",     lty, rty),
        BinaryOpKind::Rem => format!("cannot mod `{}` by `{}`",        lty, rty),
        _ => unreachable!(),
    };
    let aderr = AdError::new(AdErrorKind::Deny, Some(23), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// no implementation for `{}` {op} `{}`
pub fn e0024(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    kind: BinaryOpKind,
    lty: &Type,
    rty: &Type,
){
    let message = format!("no implementation for `{}` {} `{}`", lty, kind, rty);
    let aderr = AdError::new(AdErrorKind::Deny, Some(24), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// use of possibly-uninitialized variable: `{}`
pub fn e0027(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ident:  &str,
){
    let message = format!("use of possibly-uninitialized variable: `{}`", ident);
    let aderr = AdError::new(AdErrorKind::Deny, Some(27), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// cannot assign twice to immutable variable: `{}`
pub fn e0028(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    ident:  &str,
){
    let message = format!("cannot assign twice to immutable variable: `{}`", ident);
    let aderr = AdError::new(AdErrorKind::Deny, Some(28), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

/// this function takes {} argument[s] but {} argument[s] were supplied
pub fn e0029(
    errors: Rc<RefCell<Errors>>,
    (path, lines, token): (&str, &[&str], &[Token]),
    expect: usize,
    actual: usize,
){
    let message = format!("this function takes {expect} argument but {actual} argument were supplied");
    let aderr = AdError::new(AdErrorKind::Deny, Some(29), message, nearby(path, lines, token).unwrap_or_default());
    errors.borrow_mut().push(aderr);
}

fn nearby(path: &str, lines: &[&str], token: &[Token]) -> Result<String, ()> {
    let begin  = &token.get(0).ok_or(())?;
    let end    = &token.last().ok_or(())?;
    let digits = usize::digits(end.line);
    let mut message = String::new();

    message += &format!("\x1b[34m{1:>0$}-->\x1b[0m {2}:{3}:{4}\n", digits, "", path, begin.line, begin.cur.start);
    if begin.line == end.line {
        let target_len = end.cur.end - begin.cur.start;
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            begin.cur.start,
            "^".repeat(target_len));
    } else if end.line - begin.line < 10 {
        message += "[TODO: multiple line error message]\n";
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m\n",
            digits, "",
            begin.cur.start,
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, line, lines.get(line-1).ok_or(())?);
        }
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            end.cur.start,
            "^".repeat(end.kind.to_string().len()));
    } else {
        message += "[TODO: multiple line split error message]\n";
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, begin.line, lines.get(begin.line-1).ok_or(())?);
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m\n",
            digits, "",
            begin.cur.start,
            "^".repeat(begin.kind.to_string().len()));
        for line in begin.line+1..=end.line {
            message += &format!("\x1b[34m{1:>0$} |\x1b[0m {2}\n", digits, line, lines.get(line-1).ok_or(())?);
        }
        message += &format!("\x1b[34m{1:>0$} |\x1b[0m {1:2$}\x1b[31m{3}\x1b[0m",
            digits, "",
            end.cur.start,
            "^".repeat(end.kind.to_string().len()));
    }

    Ok(message)
}
