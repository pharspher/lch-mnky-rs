use crate::token::Token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position..].chars().next().unwrap();
            self.position = self.read_position;
            self.read_position += 1;
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let token = match self.ch {
            '=' => Token::Assign,
            '+' => Token::Plus,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '\0' => Token::EOF,

            _ if self.is_letter(self.ch) => {
                let identifier = self.read_literal();
                Token::match_keyword(identifier)
                    .unwrap_or(Token::Identifier(identifier.to_string()))
            }

            _ if self.ch.is_numeric() => {
                let int_lit = self.read_number();
                Token::Int(int_lit.to_string())
            }

            _ => Token::Illegal,
        };

        self.read_char();
        token
    }

    fn skip_whitespaces(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn read_literal(&mut self) -> &str {
        let start = self.position;

        let mut next_char = self.peek_char();
        while self.is_letter(next_char) {
            self.read_char();
            next_char = self.peek_char();
        }

        &self.input[start..self.read_position]
    }

    fn read_number(&mut self) -> &str {
        let start = self.position;

        let mut next_char = self.peek_char();
        while next_char.is_numeric() {
            self.read_char();
            next_char = self.peek_char();
        }

        &self.input[start..self.read_position]
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position..].chars().next().unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from(
            r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
        "#,
        );

        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("five")));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Int(String::from("5")));
        assert_eq!(lexer.next_token(), Token::SemiColon);

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("ten")));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Int(String::from("10")));
        assert_eq!(lexer.next_token(), Token::SemiColon);

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("add")));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Fn);
        assert_eq!(lexer.next_token(), Token::LeftParen);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("x")));
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("y")));
        assert_eq!(lexer.next_token(), Token::RightParen);
        assert_eq!(lexer.next_token(), Token::LeftBrace);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("x")));
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("y")));
        assert_eq!(lexer.next_token(), Token::SemiColon);
        assert_eq!(lexer.next_token(), Token::RightBrace);
        assert_eq!(lexer.next_token(), Token::SemiColon);

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(
            lexer.next_token(),
            Token::Identifier(String::from("result"))
        );
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("add")));
        assert_eq!(lexer.next_token(), Token::LeftParen);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("five")));
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Identifier(String::from("ten")));
        assert_eq!(lexer.next_token(), Token::RightParen);
        assert_eq!(lexer.next_token(), Token::SemiColon);
    }
}
