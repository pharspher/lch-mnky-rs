use crate::ast::Precedence::Prefix;
use crate::ast::{
    BlockStmt, BoolLiteral, Expr, ExprStmt, IdentExpr, IfExpr, InfixExpr, IntLiteral, LetStmt,
    Precedence, PrefixExpr, Program, ReturnStmt, Stmt,
};
use crate::parser::ParseError::{InvalidExpr, InvalidStmt};
use crate::{enter, info};
use lexer::lexer::Lexer;
use lexer::token::Token;
use log::warn;
#[cfg(feature = "serial-test")]
use serial_test::serial;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    InvalidToken(String),
    InvalidStmt(String),
    InvalidExpr(String),
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr: Option<Token>,
    next: Option<Token>,
}
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        enter!("[Program]");
        info!("Input: {}", lexer.input);
        let mut parser = Parser {
            lexer,
            curr: None,
            next: None,
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        enter!("[Program]");
        let mut program = Program::new();

        while self.curr.is_some() {
            let curr = self
                .curr
                .or_error(ParseError::InvalidToken("None".to_string()))?;

            info!("Process [{}]", curr);

            match curr {
                Token::EOF => break,
                _ => {
                    let stmt = self.parse_stmt()?;
                    info!("Parsed statement: {}", stmt);
                    program.stmts.push(stmt);
                }
            }

            self.advance();
        }

        Ok(program)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        enter!("[Stmt]");
        self.log_position();

        match self.curr {
            None => Err(InvalidStmt(
                "No current token available to parse statement".to_string(),
            )),
            Some(Token::Let) => self.parse_let_stmt(),
            Some(Token::Return) => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParseError> {
        enter!("[LetStmt]");
        self.log_position();

        assert!(
            self.is_curr(Token::Let),
            "Expect self.curr_token to be Token::Let, found: {:?}",
            self.curr
        );

        self.advance();
        let identifier = match self.curr.as_ref() {
            Some(Token::Identifier(__)) => IdentExpr::new(self.curr.take_and_log().unwrap()),
            _ => return Err(InvalidStmt("Expected identifier after 'let'".to_string())),
        };

        self.advance();
        if !self.is_curr(Token::Assign) {
            return Err(InvalidStmt("Expected '=' after identifier".to_string()));
        }

        self.advance();
        let expr = self.parse_expr(Precedence::Lowest)?;

        if self.is_next(Token::SemiColon) {
            self.advance();
        }

        let parsed_stmt = Stmt::Let(LetStmt::new(Token::Let, identifier, expr));
        info!("Parsed let statement: {}", parsed_stmt);
        Ok(parsed_stmt)
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        enter!("[ReturnStmt]");
        self.log_position();

        assert!(
            self.is_curr(Token::Return),
            "Expect self.curr_token to be Token::Return, found: {:?}",
            self.curr
        );

        self.advance();
        let expr = self.parse_expr(Precedence::Lowest)?;

        if self.is_next(Token::SemiColon) {
            self.advance();
        }

        Ok(Stmt::Return(ReturnStmt::new(Token::Return, expr)))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        enter!("[ExprStmt]");
        self.log_position();

        let stmt = self
            .parse_expr(Precedence::Lowest)
            .map(|exp| Stmt::Expression(ExprStmt::new(exp)));

        if self.is_next(Token::SemiColon) {
            self.advance();
        }

        stmt
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        enter!("[Expr]");
        self.log_position();

        let token = self.curr.as_ref().expect("Expect current token to be Some");

        let mut left_prefix = match token {
            Token::Identifier(_) => self.parse_ident(),
            Token::Int(_) => self.parse_int_literal(),
            Token::True | Token::False => self.parse_bool_literal(),
            Token::Bang | Token::Minus => self.parse_prefix(),
            Token::LeftParen => self.parse_group(),
            Token::If => self.parse_if_expr(),
            _ => {
                return Err(InvalidExpr(format!(
                    "No expression parser implemented for token: {:?}",
                    token
                )));
            }
        }?;

        while !self.is_next(Token::SemiColon) {
            if let Some(ref next_token) = self.next {
                let next_precedence = Self::get_precedence(next_token);
                if precedence >= next_precedence {
                    break;
                }
            }

            self.advance();
            match self.curr {
                Some(Token::Plus)
                | Some(Token::Minus)
                | Some(Token::Asterisk)
                | Some(Token::Slash)
                | Some(Token::EQ)
                | Some(Token::NotEQ)
                | Some(Token::LT)
                | Some(Token::GT) => {
                    let curr_token = self.curr.take().unwrap();
                    left_prefix = self.parse_infix(curr_token, left_prefix)?
                }
                _ => break,
            }
        }
        info!("Final left_prefix: [{}]", left_prefix.to_string());
        Ok(left_prefix)
    }

    fn parse_ident(&mut self) -> Result<Expr, ParseError> {
        enter!("[Ident]");
        self.log_position();

        if let Some(Token::Identifier(_)) = &self.curr {
            Ok(Expr::Ident(IdentExpr::new(self.curr.take().unwrap())))
        } else {
            Err(InvalidExpr("Expected identifier".to_string()))
        }
    }

    fn parse_int_literal(&mut self) -> Result<Expr, ParseError> {
        enter!("[IntLiteral]");
        self.log_position();

        if let Some(Token::Int(value_str)) = &self.curr {
            value_str
                .parse::<i64>()
                .map(|value| Expr::Int(IntLiteral::new(self.curr.take().unwrap(), value)))
                .map_err(|error| InvalidExpr(error.to_string()))
        } else {
            Err(InvalidExpr(format!(
                "Expected integer literal, found: {:?}",
                self.curr.as_log_string()
            )))
        }
    }

    fn parse_bool_literal(&mut self) -> Result<Expr, ParseError> {
        enter!("[BoolLiteral]");
        self.log_position();

        match self.curr {
            Some(Token::True) => Ok(Expr::Bool(BoolLiteral::new(Token::True, true))),
            Some(Token::False) => Ok(Expr::Bool(BoolLiteral::new(Token::False, false))),
            _ => Err(InvalidExpr(format!(
                "Expected boolean literal, found: {:?}",
                self.curr.as_log_string()
            ))),
        }
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        enter!("[Prefix]");
        self.log_position();

        assert!(
            self.is_curr(Token::Bang) || self.is_curr(Token::Minus),
            "Expect self.curr_token to be Token::Bang or Token::Minus, found: {:?}",
            self.curr
        );
        let token = self.curr.take_and_log().unwrap();

        self.advance();
        self.parse_expr(Prefix)
            .map(|right_expr| Expr::Prefix(PrefixExpr::new(token, right_expr)))
    }

    fn parse_group(&mut self) -> Result<Expr, ParseError> {
        enter!("[Group]");
        self.log_position();

        assert!(
            self.is_curr(Token::LeftParen),
            "Expect self.curr_token to be Token::LeftParen, found: {:?}",
            self.curr
        );

        self.advance();
        let expr = self.parse_expr(Precedence::Lowest);

        info!("Parsed group expression: {}", expr.as_ref().unwrap());
        if !self.is_next(Token::RightParen) {
            return Err(InvalidExpr(format!(
                "Expected right parenthesis, found: {:?}",
                self.next
            )));
        }

        self.advance();
        expr
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        enter!("[IfExpr]");
        self.log_position();

        assert!(
            self.is_curr(Token::If),
            "Expect self.curr_token to be Token::If, found: {:?}",
            self.curr
        );

        self.advance();
        let condition = self.parse_expr(Precedence::Lowest);
        info!("Parsed condition: {}", condition.as_ref().unwrap());

        if !self.is_next(Token::LeftBrace) {
            return Err(InvalidExpr(format!(
                "Expected {{ after if condition, found: {:?}",
                self.next
            )));
        }

        self.advance();
        let consequence = self.parse_block_stmt();

        let alternative = if self.is_next(Token::Else) {
            self.advance();
            if !self.is_next(Token::LeftBrace) {
                return Err(InvalidExpr(format!(
                    "Expected {{ after else, found: {:?}",
                    self.next
                )));
            }
            self.parse_block_stmt().map(|block| Some(block))
        } else {
            Ok(None)
        };

        Ok(Expr::If(IfExpr::new(
            condition?,
            consequence?,
            alternative?,
        )))
    }

    fn parse_block_stmt(&mut self) -> Result<BlockStmt, ParseError> {
        enter!("[BlockStmt]");
        self.log_position();

        self.advance();

        let mut block_stmts = Vec::new();

        while !self.is_curr(Token::RightBrace) && !self.is_curr(Token::EOF) {
            if let Ok(stmt) = self.parse_stmt() {
                info!("Parsed block statement: {}", stmt);
                block_stmts.push(stmt);
            }
            self.advance();
        }

        Ok(BlockStmt::new(block_stmts))
    }

    fn parse_infix(&mut self, token: Token, left: Expr) -> Result<Expr, ParseError> {
        enter!("[Infix]");
        self.log_position();

        info!("Token: {token}, left_expr: {:?}", left.to_string());

        self.advance();
        self.parse_expr(Self::get_precedence(&token))
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

    fn advance(&mut self) {
        self.curr = self.next.take();
        self.next = Some(self.lexer.next_token());
    }

    fn log_position(&self) {
        info!(
            "curr: [{}], next: [{}]",
            self.curr.as_log_string(),
            self.next.as_log_string()
        );
    }

    fn is_curr(&self, token: Token) -> bool {
        self.curr.is_token(token)
    }

    fn is_next(&self, token: Token) -> bool {
        self.next.is_token(token)
    }
}

pub trait OptionToken<'a> {
    fn is_token(&self, token: Token) -> bool;
    fn or_error(&self, error: ParseError) -> Result<&Token, ParseError>;
    fn as_log_string(&self) -> String;
}
impl<'a> OptionToken<'a> for Option<Token> {
    fn is_token(&self, token: Token) -> bool {
        match self {
            Some(tok) => *tok == token,
            None => false,
        }
    }

    fn or_error(&self, error: ParseError) -> Result<&Token, ParseError> {
        self.as_ref().ok_or(error)
    }

    fn as_log_string(&self) -> String {
        match self {
            Some(tok) => format!("{:?}", tok),
            None => "".to_string(),
        }
    }
}

pub trait TakeAndLogToken {
    fn take_and_log(&mut self) -> Option<Token>;
}
impl TakeAndLogToken for Option<Token> {
    fn take_and_log(&mut self) -> Option<Token> {
        match self {
            Some(tok) => {
                info!("Take {:?}", tok);
                self.take()
            }
            None => {
                warn!("No token available to take");
                None
            }
        }
    }
}

#[cfg_attr(feature = "serial-test", serial)]
#[cfg(test)]
mod test {
    use crate::ast::{ExprStmt, Program, Stmt};
    use crate::init_logger;
    use crate::parser::ParseError;
    use crate::parser::Parser;
    use crate::test_utils::new_bool;
    use crate::test_utils::new_expr_stmt;
    use crate::test_utils::{
        new_ident, new_ident_expr, new_infix_expr, new_int, new_let_stmt, new_ret_stmt,
    };
    use lexer::lexer::Lexer;
    use lexer::token::Token;

    #[test]
    fn test_let_stmt() {
        init_logger();

        let input = r"
            let x = 5;
            let y = 10;
            let result = x + y;";
        let program = parse_program(input);

        let expected_ident = vec![
            new_let_stmt(new_ident("x"), new_int(5)),
            new_let_stmt(new_ident("y"), new_int(10)),
            new_let_stmt(
                new_ident("result"),
                new_infix_expr(new_ident_expr("x"), Token::Plus, new_ident_expr("y")),
            ),
        ];

        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );
        let program = program.unwrap();
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

        let input = "return x + y;";
        let program = parse_program(input);
        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );

        let program = program.unwrap();
        assert_eq!(program.stmts.len(), 1);

        let stmt = program.stmts.first();
        assert!(stmt.is_some());

        assert_eq!(
            stmt.unwrap(),
            &new_ret_stmt(new_infix_expr(
                new_ident_expr("x"),
                Token::Plus,
                new_ident_expr("y")
            )),
        );
    }

    #[test]
    fn test_ident_expr() {
        init_logger();

        let input = "foobar";
        let program = parse_program(input);
        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );

        let program = program.unwrap();
        assert_eq!(program.stmts.len(), 1);

        let expr_stmt = if let Some(Stmt::Expression(stmt)) = program.stmts.first() {
            Some(stmt)
        } else {
            None
        };
        assert!(expr_stmt.is_some());

        assert_eq!(*expr_stmt.unwrap(), ExprStmt::new(new_ident_expr("foobar")))
    }

    #[test]
    fn test_int_literal() {
        init_logger();

        let input = "5;";
        let program = parse_program(input);
        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );

        let program = program.unwrap();
        assert_eq!(program.stmts.len(), 1);

        let expr_stmt = if let Some(Stmt::Expression(stmt)) = program.stmts.first() {
            Some(stmt)
        } else {
            None
        };
        assert!(expr_stmt.is_some());

        assert_eq!(*expr_stmt.unwrap(), ExprStmt::new(new_int(5)));
    }

    #[test]
    fn test_bool_literal() {
        init_logger();

        let input = r"
            true;
            true == false;
            ";
        let program = parse_program(input);
        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );

        let program = program.unwrap();
        assert_eq!(program.stmts.len(), 2);

        let first_stmt = program.stmts.first().unwrap();
        assert_eq!(first_stmt.to_string(), "true;");
        assert_eq!(*first_stmt, new_expr_stmt(new_bool(true)));

        let second_stmt = program.stmts.get(1).unwrap();
        assert_eq!(second_stmt.to_string(), "(true) == (false);");
        assert_eq!(
            *second_stmt,
            new_expr_stmt(new_infix_expr(new_bool(true), Token::EQ, new_bool(false))),
        );
    }

    #[test]
    fn test_prefix_expr() {
        init_logger();

        let input = r"
            !15;
            -x;";

        let program = parse_program(input);
        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );

        let program = program.unwrap();
        assert_eq!(program.stmts.len(), 2);

        assert_eq!("!(15);", program.stmts.first().unwrap().to_string());
        assert_eq!("-(x);", program.stmts.get(1).unwrap().to_string());
    }

    #[test]
    fn test_infix_expr() {
        init_logger();

        let input = r"
        a + b * c + d / e - f;
        3 + 4 * 5 == 3 * 1 + 4 * 5;
        ";

        let program = parse_program(input);
        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );

        let program = program.unwrap();
        assert_eq!(program.stmts.len(), 2);

        let expect = "(((a) + ((b) * (c))) + ((d) / (e))) - (f);";
        assert_eq!(expect, program.stmts.first().unwrap().to_string());

        let expect = "((3) + ((4) * (5))) == (((3) * (1)) + ((4) * (5)));";
        assert_eq!(expect, program.stmts.get(1).unwrap().to_string());
    }

    #[test]
    fn test_group_expr() {
        init_logger();

        let input = r"
            a + b * (c + d) / e - f;
            -(5 + 5);
            ";

        let program = parse_program(input);
        assert!(
            program.is_ok(),
            "Failed to parse program: {:?}",
            program.err()
        );

        let program = program.unwrap();
        assert_eq!(program.stmts.len(), 2);

        let expect = "((a) + (((b) * ((c) + (d))) / (e))) - (f);";
        assert_eq!(expect, program.stmts.first().unwrap().to_string());

        let expect = "-((5) + (5));";
        assert_eq!(expect, program.stmts.get(1).unwrap().to_string());
    }

    // //#[test]
    // fn test_if_expr() {
    //     init_logger();
    //
    //     let input = r"
    //         if (x < y) {
    //             let x = 3;
    //             x;
    //         } else {
    //             y;
    //         }
    //         ";
    //
    //     let program = parse_program(input);
    //     assert_eq!(program.stmts.len(), 1);
    //
    //     let stmt = program.stmts.first().unwrap();
    //     assert_eq!(
    //         stmt.to_string(),
    //         "if ((x) < (y)) { let x = 3; x; } else { y; }"
    //     );
    // }

    fn parse_program(input: &str) -> Result<Program, ParseError> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }
}
