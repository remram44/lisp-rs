use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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
    Builtin(fn(&Vec<Element>) -> Element),
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
    fn apply(&self, args: &Vec<Element>) -> Element {
        match self {
            Function::Builtin(func) => func(args),
            Function::Defined(func) => {
                if args.len() != func.arg_names.len() {
                    panic!("Wrong number of arguments to lambda function");
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
    Builtin(fn(&Vec<Element>, Environment) -> Element),
    Defined(Rc<DefinedMacro>),
}

pub struct DefinedMacro {
    arg_names: Vec<String>,
    body: Element,
    env: Environment,
}

impl Macro {
    fn apply(&self, expr: &Vec<Element>, env: Environment) -> Element {
        match self {
            Macro::Builtin(func) => func(expr, env),
            Macro::Defined(macro_) => {
                if expr.len() != macro_.arg_names.len() + 1 {
                    panic!("Wrong number of arguments to macro");
                }

                // Call the macro body, in its definition's environment
                let mut macro_env = (*macro_.env).clone();
                for (name, value) in macro_.arg_names.iter().zip(expr[1..].iter()) {
                    macro_env.insert(name.clone(), EnvItem::Value(value.clone()));
                }
                let code = eval(&macro_.body, Rc::new(macro_env));

                // Now evaluate the result, in the caller's environment
                eval(&code, env)
            }
        }
    }
}

pub fn eval(expr: &Element, env: Environment) -> Element {
    match expr {
        Element::Atom(atom) => {
            match env.get(atom) {
                None => panic!("Unbound atom {}", atom),
                Some(EnvItem::Value(value)) => value.clone(),
                Some(EnvItem::Macro(_)) => panic!("Can't use macro {} in this context", atom),
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
        Element::Function(_) => panic!("Found function"),
    }
}

fn call_func(expr: &Vec<Element>, env: Environment) -> Element {
    let expr0 = match expr.get(0) {
        Some(e) => e,
        None => panic!("Empty list for call"),
    };
    match eval(expr0, env.clone()) {
        Element::Atom(atom) => panic!("Attempt to call atom {}", atom),
        Element::List(_) => panic!("Attempt to call list"),
        Element::Function(func) => {
            let args = expr[1..].iter()
                .map(|arg| eval(arg, env.clone()))
                .collect();
            func.apply(&args)
        }
    }
}

fn quote(expr: &Vec<Element>, _env: Environment) -> Element {
    if expr.len() != 2 {
        panic!("Wrong number of arguments to quote");
    }
    expr[1].clone()
}

fn set(expr: &Vec<Element>, env: Environment) -> Element {
    if expr.len() < 4 || expr.len() % 2 != 0 {
        panic!("Wrong number of arguments to set: {}", expr.len() - 1);
    }
    let mut new_env = (*env).clone();
    let mut i = 1;
    while i + 1 < expr.len() {
        match &expr[i] {
            Element::Atom(atom) => {
                new_env.insert(atom.clone(), EnvItem::Value(eval(&expr[i + 1], env.clone())));
            }
            Element::List(_) => panic!("Cannot set a list"),
            Element::Function(_) => panic!("Cannot set a function"),
        }
        i += 2;
    }
    eval(&expr[expr.len() - 1], Rc::new(new_env))
}

fn lambda(expr: &Vec<Element>, env: Environment) -> Element {
    if expr.len() != 3 {
        panic!("Wrong number of arguments to lambda");
    }
    let arg_names = match &expr[1] {
        Element::List(list) => list,
        _ => panic!("Wrong syntax for lambda arguments list"),
    };
    let arg_names: Vec<String> = arg_names.iter()
        .map(|a| match a {
            Element::Atom(atom) => atom.clone(),
            _ => panic!("Wrong syntax for lambda argument"),
        })
        .collect();
    let body = expr[2].clone();
    Element::Function(Function::Defined(Rc::new(
        DefinedFunction { arg_names, body, env },
    )))
}

fn defmacro(expr: &Vec<Element>, env: Environment) -> Element {
    if expr.len() != 5 {
        panic!("Wrong number of arguments to defmacro");
    }
    let name = match &expr[1] {
        Element::Atom(atom) => atom.clone(),
        _ => panic!("Wrong syntax for defmacro name"),
    };
    let arg_names = match &expr[2] {
        Element::List(list) => list,
        _ => panic!("Wrong syntax for defmacro arguments list"),
    };
    let arg_names: Vec<String> = arg_names.iter()
        .map(|a| match a {
            Element::Atom(atom) => atom.clone(),
            _ => panic!("Wrong syntax for defmacro argument"),
        })
        .collect();
    let body = expr[3].clone();
    let mut new_env = (*env).clone();
    let macro_ = Macro::Defined(Rc::new(
        DefinedMacro { arg_names, body, env },
    ));
    new_env.insert(name, EnvItem::Macro(macro_));
    eval(&expr[4], Rc::new(new_env))
}

fn cons(args: &Vec<Element>) -> Element {
    if args.len() != 2 {
        panic!("Wrong number of arguments to cons");
    }
    match args[1] {
        Element::List(ref list) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend_from_slice(list);
            Element::List(new_list)
        }
        Element::Atom(ref atom) => panic!("Attempt to cons with atom {}", atom),
        Element::Function(_) => panic!("Attempt to cons with a function"),
    }
}

pub fn default_environment() -> Environment {
    let mut env = HashMap::new();
    env.insert("quote".to_owned(), EnvItem::Macro(Macro::Builtin(quote)));
    env.insert("set".to_owned(), EnvItem::Macro(Macro::Builtin(set)));
    env.insert("lambda".to_owned(), EnvItem::Macro(Macro::Builtin(lambda)));
    env.insert("defmacro".to_owned(), EnvItem::Macro(Macro::Builtin(defmacro)));
    env.insert("cons".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(cons))));
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
        ),
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
        ),
        List(vec![atom("a")]),
    );

    // (cons (quote a) (quote (b c))) -> (a b c)
    assert_eq!(
        eval(
            &List(vec![atom("cons"), List(vec![atom("quote"), atom("a")]), List(vec![atom("quote"), List(vec![atom("b"), atom("c")])])]),
            default_environment(),
        ),
        List(vec![atom("a"), atom("b"), atom("c")]),
    );
}

#[test]
fn test_set() {
    // (set a (quote b) a) -> b
    assert_eq!(
        eval(
            &List(vec![atom("set"), atom("a"), List(vec![atom("quote"), atom("b")]), atom("a")]),
            default_environment(),
        ),
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
        ),
        atom("d"),
    );
}

#[test]
fn test_set_lambda() {
    // (set f (lambda (a b) b) (f (quote d) (quote e))) -> e
    assert_eq!(
        eval(
            &List(vec![atom("set"), atom("f"), List(vec![atom("lambda"), List(vec![atom("a"), atom("b")]), atom("b")]), List(vec![atom("f"), List(vec![atom("quote"), atom("d")]), List(vec![atom("quote"), atom("e")])])]),
            default_environment(),
        ).unwrap(),
        atom("e"),
    );
}

#[test]
fn test_defmacro() {
    // (cons (quote quote) (quote (a))) -> (quote a)
    assert_eq!(
        eval(
            &List(vec![atom("cons"), List(vec![atom("quote"), atom("quote")]), List(vec![atom("quote"), List(vec![atom("a")])])]),
            default_environment(),
        ),
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
        ),
        atom("a"),
    );
}
