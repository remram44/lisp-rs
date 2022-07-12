use std::collections::HashMap;
use std::rc::Rc;

use crate::{DefinedFunction, DefinedMacro, Element, Environment, EnvItem, Function, Macro, ProgramError, eval};

/*
 * Macros
 */

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

/*
 * Functions
 */

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

    // Macros
    env.insert("quote".to_owned(), EnvItem::Macro(Macro::Builtin(quote)));
    env.insert("let".to_owned(), EnvItem::Macro(Macro::Builtin(let_)));
    env.insert("lambda".to_owned(), EnvItem::Macro(Macro::Builtin(lambda)));
    env.insert("if".to_owned(), EnvItem::Macro(Macro::Builtin(if_)));
    env.insert("defmacro".to_owned(), EnvItem::Macro(Macro::Builtin(defmacro)));
    // Functions
    env.insert("cons".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(cons))));
    env.insert("car".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(car))));
    env.insert("cdr".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(cdr))));
    env.insert("eq".to_owned(), EnvItem::Value(Element::Function(Function::Builtin(eq))));
    // Items
    env.insert("nil".to_owned(), EnvItem::Value(Element::List(Vec::new())));

    return Rc::new(env);
}
