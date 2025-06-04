use std::fmt;
use std::fmt::Error;

use lexer::token::Token;

pub struct Program {
    pub stmts: Vec<Stmt>,
}
impl Default for Program {
    fn default() -> Self {
        Program::new()
    }
}
impl Program {
    pub fn new() -> Self {
        Program { stmts: Vec::new() }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Let(LetStmt),
    Return(ReturnStmt),
    Expression(ExprStmt),
}
impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let(let_stmt) => write!(f, "{};", let_stmt),
            Stmt::Return(return_stmt) => write!(f, "{}", return_stmt),
            Stmt::Expression(expr_stmt) => write!(f, "{}", expr_stmt),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStmt {
    token: Token,
    name: IdentExpr,
    value: Expr,
}
impl LetStmt {
    pub fn new(token: Token, ident: IdentExpr, expr: Expr) -> Self {
        Self {
            token,
            name: ident,
            value: expr,
        }
    }
}
impl fmt::Display for LetStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Token::Identifier(ident) = &self.name.token {
            write!(f, "let {} = {};", ident, self.value)
        } else {
            Err(Error)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    token: Token,
    expr: Expr,
}
impl ReturnStmt {
    pub fn new(token: Token, expr: Expr) -> Self {
        Self { token, expr }
    }
}
impl fmt::Display for ReturnStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.expr)
    }
}

#[derive(Debug, PartialEq)]
pub struct ExprStmt {
    token: Token,
    expr: Expr,
}
impl ExprStmt {
    pub fn new(token: Token, expr: Expr) -> Self {
        Self { token, expr }
    }
}
impl fmt::Display for ExprStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{};", self.expr)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Ident(IdentExpr),
    Int(IntLiteral),
    Prefix(PrefixExpr),
    Infix(InfixExpr),
    NoImpl,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ident) => write!(f, "{}", ident),
            Expr::Int(int) => write!(f, "{}", int),
            Expr::Prefix(prefix) => write!(f, "{}", prefix),
            Expr::Infix(infix) => write!(f, "{}", infix),
            Expr::NoImpl => write!(f, "NoImpl"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentExpr {
    token: Token,
}
impl IdentExpr {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}
impl fmt::Display for IdentExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Token::Identifier(ident) = &self.token {
            write!(f, "{}", ident)
        } else {
            Err(Error)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntLiteral {
    token: Token,
    value: i64,
}
impl IntLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}
impl fmt::Display for IntLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Token::Int(value) = &self.token {
            write!(f, "{}", value)
        } else {
            Err(Error)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpr {
    token: Token,
    expr: Box<Expr>,
}
impl PrefixExpr {
    pub fn new(token: Token, expr: Expr) -> Self {
        Self {
            token,
            expr: Box::new(expr),
        }
    }
}

impl fmt::Display for PrefixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.token, self.expr)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpr {
    token: Token,
    left_expr: Box<Expr>,
    right_expr: Box<Expr>,
}
impl InfixExpr {
    pub fn new(token: Token, left_expr: Expr, right_expr: Expr) -> Self {
        Self {
            token,
            left_expr: Box::new(left_expr),
            right_expr: Box::new(right_expr),
        }
    }
}
impl fmt::Display for InfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}) {} ({})",
            self.left_expr, self.token, self.right_expr
        )
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[cfg(test)]
mod display_tests {
    use lexer::token::Token;

    use crate::ast::{Expr, IdentExpr, InfixExpr, IntLiteral, PrefixExpr};
    use crate::init_logger;

    #[test]
    fn test_expr_display() {
        init_logger();

        // TODO: IntLiteral can only accept Token::Int, try make it explicit
        let int_expr = Expr::Int(IntLiteral::new(Token::Int("5".to_string()), 5));
        assert_eq!(int_expr.to_string(), "5");

        // TODO: IdentExpr can only accept Token::Identifier, try make it explicit
        let ident_expr = Expr::Ident(IdentExpr::new(Token::Identifier("x".to_string())));
        assert_eq!(ident_expr.to_string(), "x");

        // TODO: PrefixExpr can only accept Token::Bang or Token::Minus, try make it explicit
        let bang_prefix_expr = Expr::Prefix(PrefixExpr::new(Token::Bang, int_expr.clone()));
        assert_eq!(bang_prefix_expr.to_string(), "!(5)");

        let minus_prefix_expr = Expr::Prefix(PrefixExpr::new(Token::Minus, ident_expr.clone()));
        assert_eq!(minus_prefix_expr.to_string(), "-(x)");

        // TODO: InfixExpr can only accept Token::Plus, Token::Minus, etc., try make it explicit
        let infix_expr = Expr::Infix(InfixExpr::new(
            Token::Plus,
            bang_prefix_expr.clone(),
            minus_prefix_expr.clone(),
        ));
        assert_eq!(infix_expr.to_string(), "(!(5)) + (-(x))");

        let infix_expr = Expr::Infix(InfixExpr::new(
            Token::Asterisk,
            infix_expr.clone(),
            infix_expr.clone(),
        ));
        assert_eq!(
            infix_expr.to_string(),
            "((!(5)) + (-(x))) * ((!(5)) + (-(x)))"
        );
    }
}
