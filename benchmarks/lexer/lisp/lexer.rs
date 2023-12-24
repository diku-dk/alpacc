use std::fmt;
use std::env;
use std::io::Read;
use std::io::BufWriter;
use std::io::Write;
use logos::Logos;
use std::fs::File;
use std::time::SystemTime;
use std::fs;

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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let name = path.strip_suffix(".in").unwrap().to_string();
    let mut file = File::open(path).unwrap();
    let mut data = vec![];
    let _ = file.read_to_end(&mut data);
    let s = String::from_utf8(data).unwrap();

    let mut lex = Token::lexer(&s);
    let mut tokens = Vec::new();

    let now = SystemTime::now();
    let mut step = lex.next();
    let mut is_error = false;
    while step != None {
        match step.unwrap() {
            Ok(token) => {
                tokens.push((token, lex.span()));
            }
            Err(e) => {
                println!("Error: {e:?}");
                is_error = true;
                break;
            }
        }
        step = lex.next()
    }
    if !is_error {
        let token_count = tokens.len();
        println!{"#token: {:?}", token_count}

        match now.elapsed() {
            Ok(elapsed) => {
                println!("Time elapsed: {}μs", elapsed.as_micros());
            }
            Err(e) => {
                println!("Error: {e:?}");
            }
        }

        let outfile = name + ".out";
        fs::remove_file(outfile.clone()).ok();
        let file = File::create(outfile).ok().unwrap();
        let mut buffer = BufWriter::new(file);
        // Using Futhark binary data format.
        // https://futhark.readthedocs.io/en/latest/binary-data-format.html
        buffer.write(&['b' as u8, 2, 2])?;
        buffer.write(&" i32".as_bytes())?;
        buffer.write(&(token_count as u64).to_le_bytes())?;
        buffer.write(&3u64.to_le_bytes())?;
        for (token, range) in tokens.into_iter() {
            let token_id = match token {
                Token::Atom => { 1i32.to_le_bytes() }
                Token::LParen => { 2i32.to_le_bytes() }
                Token::RParen => { 3i32.to_le_bytes() }
            };
            buffer.write(&token_id)?;
            buffer.write(&(range.start as i32).to_le_bytes())?;
            buffer.write(&(range.end as i32).to_le_bytes())?;
        }
    }
    Ok(())
}
