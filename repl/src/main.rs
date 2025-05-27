use std::io::Write;

use lexer::lexer;
use ::lexer::token::Token;

fn main() {
    let prompt = ">> ";
    loop {
        print!("{}", prompt);
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        if std::io::stdin().read_line(&mut input).is_err() {
            eprintln!("Error reading input");
            continue;
        }

        let mut lexer = lexer::Lexer::new(input);
        loop {
            match lexer.next_token() {
                Token::EOF => break,
                token => println!("{:?}", token),
            }
        }
    }
}
