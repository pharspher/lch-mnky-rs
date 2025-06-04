use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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

    Assign,
    Plus,
    Bang,
    Minus,
    Slash,
    Asterisk,

    LT,
    GT,
    EQ,
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Bang => write!(f, "!"),
            Token::Assign => write!(f, "="),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),
            Token::EQ => write!(f, "=="),
            Token::NotEQ => write!(f, "!="),
            _ => {
                panic!("Token {:?} does not have a display implementation", self)
            }
        }
    }
}
