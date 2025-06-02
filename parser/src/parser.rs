use log::info;

use lexer::lexer::Lexer;
use lexer::token::Token;

use crate::ast::Expr::NoImpl;
use crate::ast::Precedence::Prefix;
use crate::ast::{
    Expr, ExprStmt, IdentExpr, InfixExpr, IntLiteral, LetStmt, Precedence, PrefixExpr, Program,
    ReturnStmt, Stmt,
};

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
        info!("[Program] Start parsing");
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

            self.next_token();
        }

        program
    }

    pub fn parse_let_stmt(&mut self) -> Option<Stmt> {
        let let_token = self.curr_token.take()?;

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
        if !matches!(self.curr_token, Some(Token::Assign)) {
            self.errors.push(format!(
                "Expected '=' after identifier, found: {:?}",
                self.curr_token
            ));
            return None;
        }

        while !matches!(self.curr_token, Some(Token::SemiColon))
            && !matches!(self.curr_token, Some(Token::EOF))
        {
            info!("[Let] Skip expression");
            self.next_token();
        }

        Some(Stmt::Let(LetStmt::new(let_token, identifier, NoImpl)))
    }

    pub fn parse_return_stmt(&mut self) -> Option<Stmt> {
        let return_token = self.curr_token.take().unwrap();

        self.next_token();
        while !matches!(self.curr_token, Some(Token::SemiColon))
            && !matches!(self.curr_token, Some(Token::EOF))
        {
            info!("[Return] Skip expression");
            self.next_token();
        }

        Some(Stmt::Return(ReturnStmt::new(return_token, NoImpl)))
    }

    pub fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let stmt = self.curr_token.take().and_then(|token| {
            self.parse_expr(&token, Precedence::Lowest)
                .map(|exp| Stmt::Expression(ExprStmt::new(token, exp)))
        });

        if matches!(self.next_token, Some(Token::SemiColon)) {
            self.next_token();
        }

        stmt
    }

    fn parse_expr(&mut self, token: &Token, precedence: Precedence) -> Option<Expr> {
        let mut left_prefix = match token {
            Token::Identifier(_) => self.parse_ident(token),
            Token::Int(_) => self.parse_int_literal(token),
            Token::Bang | Token::Minus => self.parse_prefix(token),
            _ => {
                self.errors.push(format!(
                    "No expression parser implemented for token: {:?}",
                    token
                ));
                None
            }
        };

        while !matches!(self.next_token, Some(Token::SemiColon)) {
            if let Some(ref next_token) = self.next_token {
                let next_precedence = Self::get_precedence(next_token);
                if precedence >= next_precedence {
                    break;
                }

                self.next_token();

                match next_token {
                    Token::Plus
                    | Token::Minus
                    | Token::Asterisk
                    | Token::Slash
                    | Token::EQ
                    | Token::NotEQ
                    | Token::LT
                    | Token::GT => left_prefix = self.parse_infix(next_token, left_prefix.unwrap()),
                    _ => break,
                }
            }
        }
        return left_prefix;
    }

    fn parse_ident(&mut self, token: &Token) -> Option<Expr> {
        Some(Expr::Ident(IdentExpr::new(token.clone())))
    }

    fn parse_int_literal(&mut self, token: &Token) -> Option<Expr> {
        if let Token::Int(value_str) = token {
            value_str
                .parse::<i64>()
                .map(|value| Expr::Int(IntLiteral::new(token.clone(), value)))
                .ok()
                .or_else(|| {
                    self.errors
                        .push(format!("Could not parse integer literal: {:?}", token));
                    None
                })
        } else {
            None
        }
    }

    fn parse_prefix(&mut self, token: &Token) -> Option<Expr> {
        self.next_token();

        self.curr_token
            .take()
            .and_then(|expr_token| self.parse_expr(&expr_token, Prefix))
            .map(|right_expr| Expr::Prefix(PrefixExpr::new(token.clone(), right_expr)))
    }

    fn parse_infix(&mut self, token: &Token, left: Expr) -> Option<Expr> {
        self.next_token();

        self.curr_token
            .take()
            .and_then(|expr_token| self.parse_expr(&expr_token, Self::get_precedence(&expr_token)))
            .map(|right_expr| Expr::Infix(InfixExpr::new(token.clone(), left, right_expr)))
    }

    fn get_precedence(token: &Token) -> Precedence {
        match token {
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash => Precedence::Product,
            Token::GT | Token::LT => Precedence::LessGreater,
            Token::EQ | Token::NotEQ => Precedence::Equals,
            _ => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod test {
    use log::info;

    use lexer::lexer::Lexer;
    use lexer::token::Token;

    use crate::ast::Expr::NoImpl;
    use crate::ast::{
        Expr, ExprStmt, IdentExpr, InfixExpr, IntLiteral, LetStmt, PrefixExpr, Program, ReturnStmt,
        Stmt,
    };
    use crate::init_logger;
    use crate::parser::Parser;

    #[test]
    fn test_let_stmt() {
        init_logger();

        let input = r"
            let x = 5;
            let y = 10;
            let result = x + y;";
        let program = parse_program(input);

        let expected_ident: Vec<LetStmt> = ["x", "y", "result"]
            .iter()
            .map(|identifier_name| {
                LetStmt::new(
                    Token::Let,
                    IdentExpr::new(Token::Identifier(identifier_name.to_string())),
                    NoImpl,
                )
            })
            .collect();

        assert_eq!(program.stmts.len(), 3);

        program.stmts.iter().enumerate().for_each(|(i, stmt)| {
            assert_eq!(
                match stmt {
                    Stmt::Let(payload) => Some(payload),
                    _ => None,
                },
                Some(&expected_ident[i])
            );
        });
    }

    #[test]
    fn test_return_stmt() {
        init_logger();

        let input = "return x;";
        let program = parse_program(input);

        assert_eq!(program.stmts.len(), 1);

        assert_eq!(program.stmts.len(), 1);
        let stmt = if let Some(Stmt::Return(return_stmt)) = program.stmts.first() {
            Some(return_stmt)
        } else {
            None
        };
        assert!(stmt.is_some());

        assert_eq!(*stmt.unwrap(), ReturnStmt::new(Token::Return, NoImpl))
    }

    #[test]
    fn test_ident_expr() {
        init_logger();

        let input = "foobar";
        let program = parse_program(input);

        assert_eq!(program.stmts.len(), 1);

        let expr_stmt = if let Some(Stmt::Expression(stmt)) = program.stmts.first() {
            Some(stmt)
        } else {
            None
        };
        assert!(expr_stmt.is_some());

        assert_eq!(
            *expr_stmt.unwrap(),
            ExprStmt::new(
                Token::Identifier("foobar".to_string()),
                Expr::Ident(IdentExpr::new(Token::Identifier("foobar".to_string())))
            )
        )
    }

    #[test]
    fn test_int_literal() {
        init_logger();

        let input = "5;";
        let program = parse_program(input);

        assert_eq!(program.stmts.len(), 1);

        let expr_stmt = if let Some(Stmt::Expression(stmt)) = program.stmts.first() {
            Some(stmt)
        } else {
            None
        };
        assert!(expr_stmt.is_some());

        assert_eq!(
            *expr_stmt.unwrap(),
            ExprStmt::new(
                Token::Int("5".to_string()),
                Expr::Int(IntLiteral::new(Token::Int("5".to_string()), 5))
            )
        )
    }

    #[test]
    fn test_prefix_expr() {
        init_logger();

        let input = r"
            !5;
            -15;";
        let program = parse_program(input);

        let actual_expr_stmts: Vec<Option<&ExprStmt>> = program
            .stmts
            .iter()
            .map(|stmt| {
                if let Stmt::Expression(stmt) = stmt {
                    Some(stmt)
                } else {
                    None
                }
            })
            .collect();

        assert_eq!(2, actual_expr_stmts.len());

        assert_eq!(
            actual_expr_stmts,
            vec![
                Some(&ExprStmt::new(
                    Token::Bang,
                    Expr::Prefix(PrefixExpr::new(
                        Token::Bang,
                        Expr::Int(IntLiteral::new(Token::Int("5".to_string()), 5))
                    ))
                )),
                Some(&ExprStmt::new(
                    Token::Minus,
                    Expr::Prefix(PrefixExpr::new(
                        Token::Minus,
                        Expr::Int(IntLiteral::new(Token::Int("15".to_string()), 15))
                    ))
                ))
            ]
        )
    }

    #[test]
    fn test_infix_expr() {
        init_logger();

        let input = vec![
            ("5 + 5;", 5, Token::Plus, 5),
            ("5 - 5;", 5, Token::Minus, 5),
            ("5 * 5;", 5, Token::Asterisk, 5),
            ("5 / 5;", 5, Token::Slash, 5),
            ("5 > 5;", 5, Token::GT, 5),
            ("5 < 5;", 5, Token::LT, 5),
            ("5 == 5;", 5, Token::EQ, 5),
            ("5 != 5;", 5, Token::NotEQ, 5),
        ];
        let program_text = input
            .iter()
            .map(|(expr, _, _, _)| expr.to_string())
            .collect::<Vec<String>>()
            .join("\n");
        info!("program text: {}", program_text);
        let program = parse_program(program_text.as_str());
        assert_eq!(8, program.stmts.len());

        let actual_expr_stmts: Vec<Option<ExprStmt>> = program
            .stmts
            .into_iter()
            .map(|stmt| {
                if let Stmt::Expression(stmt) = stmt {
                    Some(stmt)
                } else {
                    None
                }
            })
            .collect();

        assert_eq!(8, actual_expr_stmts.len());
        let expect: Vec<Option<ExprStmt>> = input
            .iter()
            .map(|(expr, left, op, right)| {
                ExprStmt::new(
                    op.clone(),
                    Expr::Infix(InfixExpr::new(
                        op.clone(),
                        Expr::Int(IntLiteral::new(Token::Int("5".to_string()), 5)),
                        Expr::Int(IntLiteral::new(Token::Int("5".to_string()), 5)),
                    )),
                )
            })
            .map(Some)
            .collect();

        assert_eq!(actual_expr_stmts, expect);
    }

    fn parse_program(input: &str) -> Program {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        info!("Stmts: {:?}", program.stmts);
        info!("Errors: {:#?}", parser.errors);
        program
    }
}
