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
    #[regex("\"([a-zA-Z0-9_ ():/@.]|-)*\"")]
    String,
    #[regex("-?[0-9]+(.[0-9]+)?([eE]([+]|-)?[0-9]+)?")]
    Num,
    #[token(r"{")]
    LCurlyParen,
    #[token(r"}")]
    RCurlyParen,
    #[token(r"[")]
    LSquareParen,
    #[token(r"]")]
    RSquareParen,
    #[token(r"true")]
    True,
    #[token(r"false")]
    False,
    #[token(r",")]
    Comma,
    #[token(r":")]
    Colon,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let token: &str = match self {
            Token::String => { "String" }
            Token::Num => { "Num" }
            Token::LCurlyParen => { "LCurlyParen" }
	        Token::RCurlyParen => { "RCurlyParen" }
	        Token::LSquareParen => { "LSquareParen" }
	        Token::RSquareParen => { "RSquareParen" }
	        Token::True => { "True" }
	        Token::False => { "False" }
            Token::Comma => { "Comma" }
            Token::Colon => { "Colon" }
        };
        write!(f, "{}", token)
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let name = path.strip_suffix(".json").unwrap().to_string();
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
		        println!("({}, {})", tokens.last().unwrap().1.start, tokens.last().unwrap().1.end);
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
                Token::String => { 1i32.to_le_bytes() }
                Token::Num => { 2i32.to_le_bytes() }
		        Token::LCurlyParen => { 3i32.to_le_bytes() }
		        Token::RCurlyParen => { 4i32.to_le_bytes() }
		        Token::LSquareParen => { 5i32.to_le_bytes() }
		        Token::RSquareParen => { 6i32.to_le_bytes() }
		        Token::True => { 7i32.to_le_bytes() }
		        Token::False => { 8i32.to_le_bytes() }
		        Token::Comma => { 9i32.to_le_bytes() }
                Token::Colon => { 10i32.to_le_bytes() }
            };
            buffer.write(&token_id)?;
            buffer.write(&(range.start as i32).to_le_bytes())?;
            buffer.write(&(range.end as i32).to_le_bytes())?;
        }
    }
    Ok(())
}
