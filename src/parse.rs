use std::fmt;

use crate::Element;

#[cfg(test)]
use crate::atom;

#[cfg(test)]
use Element::List;

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
