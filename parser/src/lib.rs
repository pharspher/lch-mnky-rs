use log::info;

use lexer::lexer::Lexer;
use lexer::token::Token;

use crate::ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement};

mod ast;

pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    next_token: Option<Token>,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            next_token: None,
            errors: Vec::new(),
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.curr_token = self.next_token.take();
        self.next_token = Some(self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while let Some(token) = &self.curr_token {
            info!("[Program]: Find {:?}", token);

            match token {
                Token::Let => {
                    if let Some(stmt) = self.parse_let_statement() {
                        info!("[Program] Resolve stmt {:?}", stmt);
                        program.statements.push(stmt);
                    }
                }
                Token::Return => {
                    if let Some(stmt) = self.parse_return_statement() {
                        info!("[Program] Resolve stmt {:?}", stmt);
                        program.statements.push(stmt);
                    }
                }
                Token::EOF => break,
                _ => {
                    self.errors
                        .push(format!("Unable to resolve statement: {:?}", token));
                }
            }

            info!("============================");
            self.next_token();
        }

        program
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let let_token = self.curr_token.take().unwrap();

        self.next_token();
        let token = self.curr_token.take();
        let identifier = if let Some(Token::Identifier(_)) = token {
            Identifier::new(token.unwrap())
        } else {
            self.errors.push(format!("Expected identifier after let, found: {:?}", token));
            return None;
        };

        self.next_token();
        if self.curr_token != Some(Token::Assign) {
            self.errors.push(format!(
                "Expected '=' after identifier, found: {:?}",
                self.curr_token
            ));
            return None;
        }
        info!("[Let] Find {:?}", self.curr_token);

        while self.curr_token != Some(Token::SemiColon) && self.curr_token != Some(Token::EOF) {
            info!("[Let] Skip expression");
            self.next_token();
        }

        Some(Box::new(LetStatement::new(let_token, identifier, Expression {})))
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let return_token = self.curr_token.take().unwrap();

        self.next_token();
        while self.curr_token != Some(Token::SemiColon) && self.curr_token != Some(Token::EOF) {
            info!("[Let] Skip expression");
            self.next_token();
        }

        Some(Box::new(ReturnStatement::new(return_token, Expression {})))
    }
}

#[cfg(test)]
mod test {
    use log::info;

    use lexer::lexer::Lexer;
    use lexer::token::Token;

    use crate::ast::{Expression, Identifier, LetStatement};
    use crate::Parser;

    #[test]
    fn test_parse_program() {
        simple_logger::SimpleLogger::new().init().unwrap();

        let input = r"
            let x = 5;
            let y = 10;
            let result = x + y;";

        let expected_identifiers: Vec<LetStatement> = ["x", "y", "result"]
            .iter()
            .map(|identifier_name| LetStatement::new(
                Token::Let,
                Identifier::new(Token::Identifier(identifier_name.to_string())),
                Expression {},
            ))
            .collect();

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        info!("Errors: {:?}", parser.errors);

        assert_eq!(program.statements.len(), 3);

        program.statements.iter().enumerate().for_each(|(i, stmt)| {
            assert_eq!(
                stmt.as_any().downcast_ref::<LetStatement>().unwrap(),
                &expected_identifiers[i]
            );
        });
    }
}
