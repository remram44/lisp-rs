use lisp::eval_string;

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
            Ok(result) => println!("{:?}", result),
            Err(error) => eprintln!("{}", error),
        }

        line.clear();
    }
}
