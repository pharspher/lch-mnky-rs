use std::collections::HashMap;
use std::mem::{Discriminant, discriminant};

use log::info;

use lexer::lexer::Lexer;
use lexer::token::Token;

use crate::ast::{Expr, ExprStmt, IdentExpr, LetStmt, Precedence, Program, ReturnStmt, Stmt};
use crate::ast::Expr::NoImpl;

type PrefixParseFn = fn(&Token) -> Option<Expr>;
type InfixParseFn = fn(Expr) -> Option<Expr>;

pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    next_token: Option<Token>,
    errors: Vec<String>,
    prefix_fns: HashMap<Discriminant<Token>, PrefixParseFn>,
    infix_fns: HashMap<Discriminant<Token>, InfixParseFn>,
}
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            next_token: None,
            errors: Vec::new(),
            prefix_fns: HashMap::new(),
            infix_fns: HashMap::new(),
        };

        parser.prefix_fns.insert(
            discriminant(&Token::Identifier("".to_string())),
            Self::parse_ident,
        );

        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) {
        self.curr_token = self.next_token.take();
        self.next_token = Some(self.lexer.next_token());
    }

    fn parse_ident(token: &Token) -> Option<Expr> {
        Some(Expr::Ident(IdentExpr::new(token.clone())))
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while let Some(token) = &self.curr_token {
            info!("[Program]: Find {:?}", token);

            match token {
                Token::Let => {
                    if let Some(stmt) = self.parse_let_stmt() {
                        info!("[Program] Resolve stmt {:?}", stmt);
                        program.stmts.push(stmt);
                    }
                }
                Token::Return => {
                    if let Some(stmt) = self.parse_return_stmt() {
                        info!("[Program] Resolve stmt {:?}", stmt);
                        program.stmts.push(stmt);
                    }
                }
                Token::EOF => break,
                _ => {
                    if let Some(stmt) = self.parse_expr_stmt() {
                        info!("[Program] Resolve stmt {:?}", stmt);
                        program.stmts.push(stmt);
                    }
                }
            }

            info!("============================");
            self.next_token();
        }

        program
    }

    pub fn parse_let_stmt(&mut self) -> Option<Stmt> {
        let let_token = self.curr_token.take().unwrap();

        self.next_token();
        let token = self.curr_token.take();
        let identifier = if let Some(Token::Identifier(_)) = token {
            IdentExpr::new(token.unwrap())
        } else {
            self.errors
                .push(format!("Expected identifier after let, found: {:?}", token));
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

        Some(Stmt::Let(LetStmt::new(let_token, identifier, NoImpl)))
    }

    pub fn parse_return_stmt(&mut self) -> Option<Stmt> {
        let return_token = self.curr_token.take().unwrap();

        self.next_token();
        while self.curr_token != Some(Token::SemiColon) && self.curr_token != Some(Token::EOF) {
            info!("[Let] Skip expression");
            self.next_token();
        }

        Some(Stmt::Return(ReturnStmt::new(return_token, NoImpl)))
    }

    pub fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let stmt = self.curr_token.take().and_then(|token| {
            self.parse_expr(&token, Precedence::Lowest)
                .map(|exp| Stmt::Expression(ExprStmt::new(token, exp)))
        });

        if self.next_token == Some(Token::SemiColon) {
            self.next_token();
        }

        stmt
    }

    fn parse_expr(&self, token: &Token, prededence: Precedence) -> Option<Expr> {
        if let Some(prefix) = self.prefix_fns.get(&discriminant(token)) {
            prefix(token)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use log::info;

    use lexer::lexer::Lexer;
    use lexer::token::Token;

    use crate::ast::{Expr, ExprStmt, IdentExpr, LetStmt, Stmt};
    use crate::ast::Expr::NoImpl;
    use crate::init_logger;
    use crate::parser::Parser;

    #[test]
    fn test_parse_program() {
        init_logger();

        let input = r"
            let x = 5;
            let y = 10;
            let result = x + y;";

        let expected_identifiers: Vec<LetStmt> = ["x", "y", "result"]
            .iter()
            .map(|identifier_name| {
                LetStmt::new(
                    Token::Let,
                    IdentExpr::new(Token::Identifier(identifier_name.to_string())),
                    NoImpl,
                )
            })
            .collect();

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        info!("Errors: {:?}", parser.errors);
        info!("Stmts: {:?}", program.stmts);

        assert_eq!(program.stmts.len(), 3);

        program.stmts.iter().enumerate().for_each(|(i, stmt)| {
            assert_eq!(
                match stmt {
                    Stmt::Let(payload) => Some(payload),
                    _ => None,
                },
                Some(&expected_identifiers[i])
            );
        });
    }

    #[test]
    fn test_identifier_expression() {
        init_logger();

        let input = "foobar";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        info!("Errors: {:?}", parser.errors);

        assert_eq!(program.stmts.len(), 1);

        let expression_statement = if let Some(Stmt::Expression(stmt)) = program.stmts.first() {
            Some(stmt)
        } else {
            None
        };
        assert!(expression_statement.is_some());

        let es = expression_statement.unwrap();
        assert_eq!(
            *es,
            ExprStmt::new(
                Token::Identifier("foobar".to_string()),
                Expr::Ident(IdentExpr::new(Token::Identifier("foobar".to_string())))
            )
        )
    }
}
