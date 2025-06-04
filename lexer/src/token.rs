use derive_more::Display;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Display)]
pub enum Token {
    Illegal,
    EOF,

    Identifier(String),
    Int(String),

    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,

    #[display("=")]
    Assign,
    #[display("+")]
    Plus,
    #[display("!")]
    Bang,
    #[display("-")]
    Minus,
    #[display("/")]
    Slash,
    #[display("*")]
    Asterisk,

    #[display("<")]
    LT,
    #[display(">")]
    GT,
    #[display("==")]
    EQ,
    #[display("!=")]
    NotEQ,

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
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "return" => Some(Token::Return),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            _ => None,
        }
    }
}
