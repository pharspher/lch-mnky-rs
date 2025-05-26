#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Illegal,
    EOF,

    Identifier(String),
    Int(String),

    Let,
    Fn,

    Assign,
    Plus,

    SemiColon,
    Comma,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
}

impl Token {
    pub fn match_keyword(input: &str) -> Option<Token> {
        match input {
            "let" => Some(Token::Let),
            "fn" => Some(Token::Fn),
            _ => None,
        }
    }
}
