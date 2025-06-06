use lexer::lexer::Lexer;
use lexer::token::Token;
use log::warn;

use crate::ast::Expr::NoImpl;
use crate::ast::Precedence::Prefix;
use crate::ast::{
    Expr, ExprStmt, IdentExpr, InfixExpr, IntLiteral, LetStmt, Precedence, PrefixExpr, Program,
    ReturnStmt, Stmt,
};
use crate::{enter, info};

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    next_token: Option<Token>,
    errors: Vec<String>,
}
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        enter!("[Program]");
        info!("Input: {}", lexer.input);
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
        enter!("[Program]");
        let mut program = Program::new();

        while let Some(ref token) = self.curr_token {
            info!("Process token {token}");

            match token {
                Token::EOF => break,
                Token::Let => {
                    if let Some(stmt) = self.parse_let_stmt() {
                        info!("Resolve let stmt {stmt}");
                        program.stmts.push(stmt);
                    }
                }
                Token::Return => {
                    if let Some(stmt) = self.parse_return_stmt() {
                        info!("Resolve return stmt {stmt}");
                        program.stmts.push(stmt);
                    }
                }
                _ => {
                    if let Some(stmt) = self.parse_expr_stmt() {
                        info!("Resolve expr stmt {stmt}");
                        program.stmts.push(stmt);
                    }
                }
            }

            self.next_token();
        }

        program
    }

    pub fn parse_let_stmt(&mut self) -> Option<Stmt> {
        enter!("[LetStmt]");
        assert!(
            matches!(self.curr_token, Some(Token::Let)),
            "Expect self.curr_token to be Token::Let, found: {:?}",
            self.curr_token
        );

        self.next_token();
        let identifier = if let Some(Token::Identifier(ref __)) = self.curr_token {
            IdentExpr::new(self.curr_token.take_and_log()?)
        } else {
            self.push_error_and_log(format!(
                "Expected identifier after let, found: {:?}",
                self.curr_token
            ));
            return None;
        };

        self.next_token();
        if !matches!(self.curr_token, Some(Token::Assign)) {
            self.push_error_and_log(format!(
                "Expected '=' after identifier, found: {:?}",
                self.curr_token
            ));
            return None;
        }

        while !matches!(self.curr_token, Some(Token::SemiColon))
            && !matches!(self.curr_token, Some(Token::EOF))
        {
            self.next_token();
        }

        Some(Stmt::Let(LetStmt::new(Token::Let, identifier, NoImpl)))
    }

    pub fn parse_return_stmt(&mut self) -> Option<Stmt> {
        enter!("[ReturnStmt]");
        assert!(
            matches!(self.curr_token, Some(Token::Return)),
            "Expect self.curr_token to be Token::Return, found: {:?}",
            self.curr_token
        );

        self.next_token();
        while !matches!(self.curr_token, Some(Token::SemiColon))
            && !matches!(self.curr_token, Some(Token::EOF))
        {
            self.next_token();
        }

        Some(Stmt::Return(ReturnStmt::new(Token::Return, NoImpl)))
    }

    pub fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        enter!("[ExprStmt]");
        let stmt = self.curr_token.take_and_log().and_then(|token| {
            self.parse_expr(&token, Precedence::Lowest)
                .map(|exp| Stmt::Expression(ExprStmt::new(token, exp)))
        });

        if matches!(self.next_token, Some(Token::SemiColon)) {
            self.next_token();
        }

        stmt
    }

    fn parse_expr(&mut self, token: &Token, precedence: Precedence) -> Option<Expr> {
        enter!("[Expr]");
        let mut left_prefix = match token {
            Token::Identifier(_) => self.parse_ident(token),
            Token::Int(_) => self.parse_int_literal(token),
            Token::Bang | Token::Minus => self.parse_prefix(token),
            _ => {
                self.push_error_and_log(format!(
                    "No expression parser implemented for token: {:?}",
                    token
                ));
                return None;
            }
        };
        info!(
            "Initial left prefix: [{}]",
            self.opt_expr_to_string(left_prefix.as_ref())
        );

        while !matches!(self.next_token, Some(Token::SemiColon)) {
            if let Some(ref next_token) = self.next_token {
                let next_precedence = Self::get_precedence(next_token);
                if precedence >= next_precedence {
                    info!("Break, {precedence} >= {next_precedence}({next_token})");
                    break;
                } else {
                    info!("Cont, {precedence} < {next_precedence}({next_token})");
                }
            }

            self.next_token();

            match self.curr_token {
                Some(Token::Plus)
                | Some(Token::Minus)
                | Some(Token::Asterisk)
                | Some(Token::Slash)
                | Some(Token::EQ)
                | Some(Token::NotEQ)
                | Some(Token::LT)
                | Some(Token::GT) => {
                    let curr_token = self.curr_token.take()?;
                    if let Some(left) = left_prefix {
                        left_prefix = self.parse_infix(curr_token, left);
                        info!(
                            "Updated left_prefix: [{}]",
                            self.opt_expr_to_string(left_prefix.as_ref())
                        );
                    } else {
                        self.push_error_and_log(format!(
                            "No left expression for infix operator: {:?}",
                            curr_token
                        ));
                        return None;
                    }
                }
                _ => break,
            }
        }
        info!(
            "Final left_prefix: [{}]",
            self.opt_expr_to_string(left_prefix.as_ref())
        );
        left_prefix
    }

    fn parse_ident(&self, token: &Token) -> Option<Expr> {
        enter!("[Ident]");
        Some(Expr::Ident(IdentExpr::new(token.clone())))
    }

    fn parse_int_literal(&mut self, token: &Token) -> Option<Expr> {
        enter!("[IntLiteral]");
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
        enter!("[Prefix]");
        info!(
            "Token: {:?}, curr[{:?}], next[{:?}]",
            token, self.curr_token, self.next_token
        );
        self.next_token();

        self.curr_token
            .take()
            .and_then(|expr_token| self.parse_expr(&expr_token, Prefix))
            .map(|right_expr| Expr::Prefix(PrefixExpr::new(token.clone(), right_expr)))
    }

    fn parse_infix(&mut self, token: Token, left: Expr) -> Option<Expr> {
        enter!("[Infix]");
        info!("Token: {token}, left_expr: {:?}", left.to_string());
        self.next_token();

        self.curr_token
            .take()
            .and_then(|expr_token| self.parse_expr(&expr_token, Self::get_precedence(&token)))
            .map(|right_expr| Expr::Infix(InfixExpr::new(token, left, right_expr)))
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

    fn push_error_and_log(&mut self, message: String) {
        warn!("Parser error: {}", &message);
        self.errors.push(message);
    }

    fn opt_expr_to_string(&self, left_prefix: Option<&Expr>) -> String {
        left_prefix
            .map(|expr| expr.to_string())
            .unwrap_or_else(|| "None".to_string())
    }
}

pub trait TakeAndLogToken {
    fn take_and_log(&mut self) -> Option<Token>;
}
impl TakeAndLogToken for Option<Token> {
    fn take_and_log(&mut self) -> Option<Token> {
        match self {
            Some(tok) => {
                info!("Token {:?} was taken", tok);
                self.take()
            }
            None => {
                warn!("No token available to take");
                None
            }
        }
    }
}

#[cfg(test)]
mod test {
    #[cfg(feature = "serial-test")]
    use crate::ast::Expr::NoImpl;
    use crate::ast::Expr::NoImpl;
    use crate::ast::{Expr, ExprStmt, IdentExpr, IntLiteral, LetStmt, Program, ReturnStmt, Stmt};
    use crate::init_logger;
    use crate::parser::Parser;
    use lexer::lexer::Lexer;
    use lexer::token::Token;
    use log::info;

    #[test]
    #[cfg_attr(feature = "serial-test", serial)]
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
    #[cfg_attr(feature = "serial-test", serial)]
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
    #[cfg_attr(feature = "serial-test", serial)]
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
    #[cfg_attr(feature = "serial-test", serial)]
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
        );
    }

    #[test]
    #[cfg_attr(feature = "serial-test", serial)]
    fn test_prefix_expr() {
        init_logger();

        let input = r"
            !15;
            -x;";

        let program = parse_program(input);
        assert_eq!(program.stmts.len(), 2);

        assert_eq!("!(15);", program.stmts.first().unwrap().to_string());
        assert_eq!("-(x);", program.stmts.get(1).unwrap().to_string());
    }

    #[test]
    #[cfg_attr(feature = "serial-test", serial)]
    fn test_infix_expr() {
        init_logger();

        let input = r"
        a + b * c + d / e - f;
        3 + 4 * 5 == 3 * 1 + 4 * 5;
        ";

        let program = parse_program(input);
        assert_eq!(program.stmts.len(), 2);

        let expect = "(((a) + ((b) * (c))) + ((d) / (e))) - (f);";
        assert_eq!(expect, program.stmts.first().unwrap().to_string());

        let expect = "((3) + ((4) * (5))) == (((3) * (1)) + ((4) * (5)));";
        assert_eq!(expect, program.stmts.get(1).unwrap().to_string());
    }

    fn parse_program(input: &str) -> Program {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        info!("Errors: {:?}", parser.errors);
        program
    }
}
