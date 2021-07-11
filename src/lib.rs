use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub struct ProgramError(String);

impl std::fmt::Display for ProgramError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for ProgramError {}

#[derive(Clone, Debug, PartialEq)]
pub enum Element {
    Atom(String),
    Function(Function),
    List(Vec<Element>),
}

#[derive(Clone)]
pub enum EnvItem {
    Macro(Macro),
    Value(Element),
}

type Environment = Rc<HashMap<String, EnvItem>>;

#[derive(Clone)]
pub enum Function {
    Builtin(fn(&Vec<Element>) -> Result<Element, ProgramError>),
    Defined(Rc<DefinedFunction>),
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin(_) => write!(f, "<builtin function>"),
            Function::Defined(_) => write!(f, "<function>"),
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        std::ptr::addr_of!(self) == std::ptr::addr_of!(other)
    }
}

pub struct DefinedFunction {
    arg_names: Vec<String>,
    body: Element,
    env: Environment,
}

impl Function {
    fn apply(&self, args: &Vec<Element>) -> Result<Element, ProgramError> {
        match self {
            Function::Builtin(func) => func(args),
            Function::Defined(func) => {
                if args.len() != func.arg_names.len() {
                    return Err(ProgramError("Wrong number of arguments to lambda function".to_owned()));
                }
                let mut new_env =  (*func.env).clone();
                for (name, value) in func.arg_names.iter().zip(args.iter()) {
                    new_env.insert(name.clone(), EnvItem::Value(value.clone()));
                }
                eval(&func.body, Rc::new(new_env))
            },
        }
    }
}

#[derive(Clone)]
pub enum Macro {
    Builtin(fn(&Vec<Element>, Environment) -> Result<Element, ProgramError>),
    Defined(Rc<DefinedMacro>),
}

pub struct DefinedMacro {
    arg_names: Vec<String>,
    body: Element,
    env: Environment,
}

impl Macro {
    fn apply(&self, expr: &Vec<Element>, env: Environment) -> Result<Element, ProgramError> {
        match self {
            Macro::Builtin(func) => func(expr, env),
            Macro::Defined(macro_) => {
                if expr.len() != macro_.arg_names.len() + 1 {
                    return Err(ProgramError("Wrong number of arguments to macro".to_owned()));
                }

                // Call the macro body, in its definition's environment
                let mut macro_env = (*macro_.env).clone();
                for (name, value) in macro_.arg_names.iter().zip(expr[1..].iter()) {
                    macro_env.insert(name.clone(), EnvItem::Value(value.clone()));
                }
                let code = eval(&macro_.body, Rc::new(macro_env))?;

                // Now evaluate the result, in the caller's environment
                eval(&code, env)
            }
        }
    }
}

pub fn eval(expr: &Element, env: Environment) -> Result<Element, ProgramError> {
    match expr {
        Element::Atom(atom) => {
            match env.get(atom) {
                None => Err(ProgramError(format!("Unbound atom {}", atom))),
                Some(EnvItem::Value(value)) => Ok(value.clone()),
                Some(EnvItem::Macro(_)) => Err(ProgramError(format!("Can't use macro {} in this context", atom))),
            }
        }
        Element::List(ref expr) => {
            // Try to call a macro
            if let Some(Element::Atom(ref atom)) = expr.get(0) {
                if let Some(EnvItem::Macro(macro_)) = env.get(atom) {
                    return macro_.apply(expr, env.clone());
                }
            }
            call_func(expr, env)
        }
        Element::Function(_) => Err(ProgramError("Found function".to_owned())),
    }
}

fn call_func(expr: &Vec<Element>, env: Environment) -> Result<Element, ProgramError> {
    let expr0 = match expr.get(0) {
        Some(e) => e,
        None => return Err(ProgramError("Empty list for call".to_owned())),
    };
    match eval(expr0, env.clone())? {
        Element::Atom(atom) => Err(ProgramError(format!("Attempt to call atom {}", atom))),
        Element::List(_) => Err(ProgramError("Attempt to call list".to_owned())),
        Element::Function(func) => {
            let mut args = Vec::new();
            for arg in &expr[1..] {
                args.push(eval(arg, env.clone())?);
            }
            func.apply(&args)
        }
    }
}

fn quote(expr: &Vec<Element>, _env: Environment) -> Result<Element, ProgramError> {
    if expr.len() != 2 {
        return Err(ProgramError("Wrong number of arguments to quote".to_owned()));
    }
    Ok(expr[1].clone())
}

fn let_(expr: &Vec<Element>, env: Environment) -> Result<Element, ProgramError> {
    if expr.len() != 3 {
        return Err(ProgramError(format!("Wrong number of arguments to set")));
    }
    let mut new_env = (*env).clone();
    let vars = match &expr[1] {
        Element::List(list) => list,
        _ => return Err(ProgramError("Wrong syntax for let bindings".to_owned())),
    };
    for var in vars {
        let (name, value) = match var {
            Element::List(list) if list.len() == 2 => (&list[0], &list[1]),
            _ => return Err(ProgramError("Wrong syntax for let binding".to_owned())),
        };
        let name = match name {
            Element::Atom(atom) => atom,
            _ => return Err(ProgramError("Wrong syntax for let bindings".to_owned())),
        };
        new_env.insert(name.clone(), EnvItem::Value(eval(&value, env.clone())?));
    }
    eval(&expr[2], Rc::new(new_env))
}

fn lambda(expr: &Vec<Element>, env: Environment) -> Result<Element, ProgramError> {
    if expr.len() != 3 {
        return Err(ProgramError("Wrong number of arguments to lambda".to_owned()));
    }
    let args_list = match &expr[1] {
        Element::List(list) => list,
        _ => return Err(ProgramError("Wrong syntax for lambda arguments list".to_owned())),
    };
    let mut arg_names = Vec::new();
    for a in args_list {
        arg_names.push(match a {
            Element::Atom(atom) => atom.clone(),
            _ => return Err(ProgramError("Wrong syntax for lambda argument".to_owned())),
        });
    }
    let body = expr[2].clone();
    Ok(Element::Function(Function::Defined(Rc::new(
        DefinedFunction { arg_names, body, env },
    ))))
}

fn if_(expr: &Vec<Element>, env: Environment) -> Result<Element, ProgramError> {
    if expr.len() < 3 || expr.len() > 4 {
        return Err(ProgramError("Wrong number of arguments to if".to_owned()));
    }
    match eval(&expr[1], env.clone())? {
        // Empty list if false
        Element::List(list) if list.len() == 0 => {
            if expr.len() >= 4 {
                eval(&expr[3], env)
            } else {
                Ok(Element::List(vec![]))
            }
        }
        // Everything else is true
        _ => eval(&expr[2], env)
    }
}

fn defmacro(expr: &Vec<Element>, env: Environment) -> Result<Element, ProgramError> {
    if expr.len() != 5 {
        return Err(ProgramError("Wrong number of arguments to defmacro".to_owned()));
    }
    let name = match &expr[1] {
        Element::Atom(atom) => atom.clone(),
        _ => return Err(ProgramError("Wrong syntax for defmacro name".to_owned())),
    };
    let args_list = match &expr[2] {
        Element::List(list) => list,
        _ => return Err(ProgramError("Wrong syntax for defmacro arguments list".to_owned())),
    };
    let mut arg_names = Vec::new();
    for a in args_list {
        arg_names.push(match a {
            Element::Atom(atom) => atom.clone(),
            _ => return Err(ProgramError("Wrong syntax for lambda argument".to_owned())),
        });
    }
    let body = expr[3].clone();
    let mut new_env = (*env).clone();
    let macro_ = Macro::Defined(Rc::new(
        DefinedMacro { arg_names, body, env },
    ));
    new_env.insert(name, EnvItem::Macro(macro_));
    eval(&expr[4], Rc::new(new_env))
}

fn cons(args: &Vec<Element>) -> Result<Element, ProgramError> {
    if args.len() != 2 {
        return Err(ProgramError("Wrong number of arguments to cons".to_owned()));
    }
    match args[1] {
        Element::List(ref list) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend_from_slice(list);
            Ok(Element::List(new_list))
        }
        Element::Atom(ref atom) => Err(ProgramError(format!("Attempt to cons with atom {}", atom))),
        Element::Function(_) => Err(ProgramError("Attempt to cons with a function".to_owned())),
    }
}

fn car(args: &Vec<Element>) -> Result<Element, ProgramError> {
    if args.len() != 1 {
        return Err(ProgramError("Wrong number of arguments to car".to_owned()));
    }
    match &args[0] {
        Element::List(list) => {
            if list.len() == 0 {
                Ok(Element::List(vec![]))
            } else {
                Ok(list[0].clone())
            }
        }
        Element::Atom(atom) => Err(ProgramError(format!("Attempt to car atom {}", atom))),
        Element::Function(_) => Err(ProgramError(format!("Attempt to car a function"))),
    }
}

fn cdr(args: &Vec<Element>) -> Result<Element, ProgramError> {
    if args.len() != 1 {
        return Err(ProgramError("Wrong number of arguments to cdr".to_owned()));
    }
    match &args[0] {
        Element::List(list) => {
            if list.len() > 1 {
                Ok(Element::List(list[1..].to_owned()))
            } else {
                Ok(Element::List(vec![]))
            }
        }
        Element::Atom(atom) => Err(ProgramError(format!("Attempt to cdr atom {}", atom))),
        Element::Function(_) => Err(ProgramError(format!("Attempt to cdr a function"))),
    }
}

fn eq(args: &Vec<Element>) -> Result<Element, ProgramError> {
    if args.len() != 2 {
        return Err(ProgramError("Wrong number of arguments to eq".to_owned()));
    }
    Ok(if args[0] == args[1] {
        Element::Atom("t".to_owned())
    } else {
        Element::List(vec![])
    })
}

pub fn default_environment() -> Environment {
    let mut env = HashMap::new();
    env.insert("quote".to_owned(), EnvItem::Macro(Macro::Builtin(quote)));
    env.insert("let".to_owned(), EnvItem::Macro(Macro::Builtin(let_)));
    env.insert("lambda".to_owned(), EnvItem::Macro(Macro::Builtin(lambda)));
    env.insert("if".to_owned(), EnvItem::Macro(Macro::Builtin(if_)));
    env.insert("defmacro".to_owned(), EnvItem::Macro(Macro::Builtin(defmacro)));
    env.insert("cons".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(cons))));
    env.insert("car".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(car))));
    env.insert("cdr".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(cdr))));
    env.insert("eq".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(eq))));
    return Rc::new(env);
}

#[cfg(test)]
fn atom(a: &str) -> Element {
    Element::Atom(a.to_owned())
}

#[cfg(test)]
use Element::List;

#[test]
fn test_quote() {
    // (quote a) -> a
    assert_eq!(
        eval(
            &List(vec![atom("quote"), atom("a")]),
            default_environment(),
        ).unwrap(),
        atom("a"),
    );
}

#[test]
fn test_cons() {
    // (cons (quote a) (quote ())) -> (a)
    assert_eq!(
        eval(
            &List(vec![atom("cons"), List(vec![atom("quote"), atom("a")]), List(vec![atom("quote"), List(vec![])])]),
            default_environment(),
        ).unwrap(),
        List(vec![atom("a")]),
    );

    // (cons (quote a) (quote (b c))) -> (a b c)
    assert_eq!(
        eval(
            &List(vec![atom("cons"), List(vec![atom("quote"), atom("a")]), List(vec![atom("quote"), List(vec![atom("b"), atom("c")])])]),
            default_environment(),
        ).unwrap(),
        List(vec![atom("a"), atom("b"), atom("c")]),
    );
}

#[test]
fn test_car_cdr() {
    // (car (quote (a b c))) -> a
    assert_eq!(
        eval(
            &List(vec![atom("car"), List(vec![atom("quote"), List(vec![atom("a"), atom("b"), atom("c")])])]),
            default_environment(),
        ).unwrap(),
        atom("a"),
    );

    // (car (quote ())) -> ()
    assert_eq!(
        eval(
            &List(vec![atom("car"), List(vec![atom("quote"), List(vec![])])]),
            default_environment(),
        ).unwrap(),
        List(vec![]),
    );

    // (cdr (quote (a b c))) -> (b c)
    assert_eq!(
        eval(
            &List(vec![atom("cdr"), List(vec![atom("quote"), List(vec![atom("a"), atom("b"), atom("c")])])]),
            default_environment(),
        ).unwrap(),
        List(vec![atom("b"), atom("c")]),
    );

    // (cdr (quote (a))) -> ()
    assert_eq!(
        eval(
            &List(vec![atom("cdr"), List(vec![atom("quote"), List(vec![atom("a")])])]),
            default_environment(),
        ).unwrap(),
        List(vec![]),
    );

    // (cdr (quote ())) -> ()
    assert_eq!(
        eval(
            &List(vec![atom("cdr"), List(vec![atom("quote"), List(vec![])])]),
            default_environment(),
        ).unwrap(),
        List(vec![]),
    );
}

#[test]
fn test_eq() {
    // (eq (quote (a b)) (quote (a b))) -> t
    assert_eq!(
        eval(
            &List(vec![atom("eq"), List(vec![atom("quote"), List(vec![atom("a"), atom("b")])]), List(vec![atom("quote"), List(vec![atom("a"), atom("b")])])]),
            default_environment(),
        ).unwrap(),
        atom("t"),
    );

    // (eq (quote (a b)) (quote (a c))) -> ()
    assert_eq!(
        eval(
            &List(vec![atom("eq"), List(vec![atom("quote"), List(vec![atom("a"), atom("b")])]), List(vec![atom("quote"), List(vec![atom("a"), atom("c")])])]),
            default_environment(),
        ).unwrap(),
        List(vec![]),
    );
}

#[test]
fn test_let() {
    // (let ((a (quote b))) a) -> b
    assert_eq!(
        eval(
            &List(vec![atom("let"), List(vec![List(vec![atom("a"), List(vec![atom("quote"), atom("b")])])]), atom("a")]),
            default_environment(),
        ).unwrap(),
        atom("b"),
    );
}

#[test]
fn test_lambda() {
    // ((lambda (a b) b) (quote c) (quote d)) -> d
    assert_eq!(
        eval(
            &List(vec![List(vec![atom("lambda"), List(vec![atom("a"), atom("b")]), atom("b")]), List(vec![atom("quote"), atom("c")]), List(vec![atom("quote"), atom("d")])]),
            default_environment(),
        ).unwrap(),
        atom("d"),
    );
}

#[test]
fn test_let_lambda() {
    // (let ((f (lambda (a b) b))) (f (quote d) (quote e))) -> e
    assert_eq!(
        eval(
            &List(vec![atom("let"), List(vec![List(vec![atom("f"), List(vec![atom("lambda"), List(vec![atom("a"), atom("b")]), atom("b")])])]), List(vec![atom("f"), List(vec![atom("quote"), atom("d")]), List(vec![atom("quote"), atom("e")])])]),
            default_environment(),
        ).unwrap(),
        atom("e"),
    );
}

#[test]
fn test_if() {
    // (if (cdr (quote (a b))) (quote c) (quote d)) -> c
    assert_eq!(
        eval(
            &List(vec![atom("if"), List(vec![atom("cdr"), List(vec![atom("quote"), List(vec![atom("a"), atom("b")])])]), List(vec![atom("quote"), atom("c")]), List(vec![atom("quote"), atom("d")])]),
            default_environment(),
        ).unwrap(),
        atom("c"),
    );

    // (if (cdr (quote (a b))) (quote c)) -> c
    assert_eq!(
        eval(
            &List(vec![atom("if"), List(vec![atom("cdr"), List(vec![atom("quote"), List(vec![atom("a"), atom("b")])])]), List(vec![atom("quote"), atom("c")])]),
            default_environment(),
        ).unwrap(),
        atom("c"),
    );

    // (if (cdr (quote (a))) (quote c) (quote d)) -> d
    assert_eq!(
        eval(
            &List(vec![atom("if"), List(vec![atom("cdr"), List(vec![atom("quote"), List(vec![atom("a")])])]), List(vec![atom("quote"), atom("c")]), List(vec![atom("quote"), atom("d")])]),
            default_environment(),
        ).unwrap(),
        atom("d"),
    );

    // (if (cdr (quote (a))) (quote c)) -> ()
    assert_eq!(
        eval(
            &List(vec![atom("if"), List(vec![atom("cdr"), List(vec![atom("quote"), List(vec![atom("a")])])]), List(vec![atom("quote"), atom("c")])]),
            default_environment(),
        ).unwrap(),
        List(vec![]),
    );
}

#[test]
fn test_defmacro() {
    // (cons (quote quote) (quote (a))) -> (quote a)
    assert_eq!(
        eval(
            &List(vec![atom("cons"), List(vec![atom("quote"), atom("quote")]), List(vec![atom("quote"), List(vec![atom("a")])])]),
            default_environment(),
        ).unwrap(),
        List(vec![atom("quote"), atom("a")]),
    );

    // (defmacro m (x) (cons (quote quote) (quote (a))) (m a)) -> a
    assert_eq!(
        eval(
            &List(vec![atom("defmacro"), atom("m"), List(vec![atom("x")]),
                List(vec![atom("cons"), List(vec![atom("quote"), atom("quote")]), List(vec![atom("quote"), List(vec![atom("a")])])]),
                List(vec![atom("m"), atom("a")]),
            ]),
            default_environment(),
        ).unwrap(),
        atom("a"),
    );
}

#[derive(Debug)]
pub struct ParseError(String);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for ParseError {}

pub fn parse(input: &str) -> Result<Element, ParseError> {
    let mut iterator = input.chars().peekable();
    parse_item(&mut iterator)
}

fn eat_whitespace<I: Iterator<Item=char>>(iterator: &mut std::iter::Peekable<I>) {
    while iterator.peek() == Some(&' ') {
        iterator.next();
    }
}

fn parse_item<I: Iterator<Item=char>>(iterator: &mut std::iter::Peekable<I>) -> Result<Element, ParseError> {
    eat_whitespace(iterator);

    // End of input reached
    if iterator.peek().is_none() {
        return Err(ParseError("End of input".to_owned()));
    }

    if iterator.peek() == Some(&'(') {
        // List
        iterator.next();
        let list = parse_list(iterator)?;
        eat_whitespace(iterator);
        iterator.next();
        Ok(Element::List(list))
    } else {
        // Atom
        parse_atom(iterator)
    }
}

fn parse_atom<I: Iterator<Item=char>>(iterator: &mut std::iter::Peekable<I>) -> Result<Element, ParseError> {
    let mut atom = String::new();
    while iterator.peek().is_some() && ![' ', '(', ')'].contains(iterator.peek().unwrap()) {
        atom.push(iterator.next().unwrap());
    }
    Ok(Element::Atom(atom))
}

fn parse_list<I: Iterator<Item=char>>(iterator: &mut std::iter::Peekable<I>) -> Result<Vec<Element>, ParseError> {
    let mut list = Vec::new();
    loop {
        match iterator.peek() {
            None => return Err(ParseError("Unterminated list".to_owned())),
            Some(&')') => break,
            _ => {
                list.push(parse_item(iterator)?);
            }
        }
        eat_whitespace(iterator);
    }
    Ok(list)
}

#[test]
fn test_parse() {
    assert_eq!(parse_atom(&mut "hello world".chars().peekable()).unwrap(), atom("hello"));
    assert_eq!(parse_atom(&mut "hello)".chars().peekable()).unwrap(), atom("hello"));

    assert_eq!(
        parse("((lambda (a b) b) (quote c) (quote  d))").unwrap(),
        List(vec![List(vec![atom("lambda"), List(vec![atom("a"), atom("b")]), atom("b")]), List(vec![atom("quote"), atom("c")]), List(vec![atom("quote"), atom("d")])]),
    );
    assert_eq!(
        parse("(  (lambda (a   b)  b) (quote c) (quote  d))  ").unwrap(),
        List(vec![List(vec![atom("lambda"), List(vec![atom("a"), atom("b")]), atom("b")]), List(vec![atom("quote"), atom("c")]), List(vec![atom("quote"), atom("d")])]),
    );
}

#[derive(Debug)]
pub enum Error {
    ParseError(ParseError),
    ProgramError(ProgramError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ParseError(e) => write!(f, "Parse error: {}", e),
            Error::ProgramError(e) => write!(f, "Program error: {}", e),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(match self {
            Error::ParseError(ref e) => e,
            Error::ProgramError(ref e) => e,
        })
    }
}

pub fn eval_string(code: &str) -> Result<Element, Error> {
    let expr = match parse(code) {
        Ok(t) => t,
        Err(e) => return Err(Error::ParseError(e)),
    };
    let result = match eval(&expr, default_environment()) {
        Ok(t) => t,
        Err(e) => return Err(Error::ProgramError(e)),
    };
    Ok(result)
}

#[test]
fn test_eval_string() {
    assert_eq!(
        eval_string("((lambda (a b) b) (quote c) (quote d))").unwrap(),
        atom("d"),
    );
}

pub fn unparse<W: std::io::Write>(expr: &Element, out: &mut W) -> std::io::Result<()> {
    match expr {
        Element::Atom(atom) => write!(out, "{}", atom),
        Element::List(list) => {
            write!(out, "(")?;
            let mut first = true;
            for element in list {
                if first {
                    first = false;
                } else {
                    write!(out, " ")?;
                }
                unparse(element, out)?;
            }
            write!(out, ")")?;
            Ok(())
        }
        Element::Function(_) => write!(out, "<function>"),
    }
}
#[test]
fn test_unparse() {
    let mut out = Vec::new();
    unparse(
        &List(vec![List(vec![atom("lambda"), List(vec![atom("a"), atom("b")]), atom("b")]), List(vec![atom("quote"), atom("c")]), List(vec![atom("quote"), atom("d")])]),
        &mut out,
    ).unwrap();
    assert_eq!(
        out,
        "((lambda (a b) b) (quote c) (quote d))".as_bytes(),
    );
}
