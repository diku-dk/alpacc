use std::fmt;
use std::env;
use std::io::Read;
use logos::Logos;
use std::fs::File;
use std::time::SystemTime;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\r]+")] // Ignore this regex pattern between tokens
enum Token {
    #[regex("[a-zA-Z0-9]+")]
    Atom,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let token: &str = match self {
            Token::Atom => { "Atom" }
            Token::LParen => { "LParen" }
            Token::RParen => { "RParen" }
        };
        write!(f, "{}", token)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(&args[1]).unwrap();
    let mut data = vec![];
    let _ = file.read_to_end(&mut data);
    let s = String::from_utf8(data).unwrap();

    let mut lex = Token::lexer(&s);
    let mut result = Vec::new();

    let now = SystemTime::now();
    let mut step = lex.next();
    while step != None {
        match step.unwrap() {
            Ok(token) => {
                result.push((token, lex.span()));
            }
            Err(e) => {
                println!("Error: {e:?}");
                break;
            }
        }
        step = lex.next()
    }
    // println!("{:?}", result);
    match now.elapsed() {
        Ok(elapsed) => {
            println!("Time elapsed: {}μs", elapsed.as_micros());
        }
        Err(e) => {
            println!("Error: {e:?}");
        }
    }
}
