use std::io::Write;
use lisp::{eval_string, unparse};

fn main() {
    let stdin = std::io::stdin();
    let mut line = String::new();
    loop {
        eprint!("> ");
        stdin.read_line(&mut line).unwrap();
        if line.len() == 0 {
            break;
        }

        line.truncate(line.trim_end().len());
        match eval_string(&line) {
            Ok(result) => {
                let mut stdout = std::io::stdout();
                unparse(&result, &mut stdout).unwrap();
                write!(stdout, "\n").unwrap();
            },
            Err(error) => eprintln!("{}", error),
        }

        line.clear();
    }
}
