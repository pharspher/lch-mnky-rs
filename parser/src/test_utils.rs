use crate::ast::{
    BoolLiteral, Expr, ExprStmt, IdentExpr, InfixExpr, IntLiteral, LetStmt, PrefixExpr, ReturnStmt,
    Stmt,
};
use lexer::token::Token;

#[cfg(test)]
pub fn new_ident(name: &str) -> IdentExpr {
    IdentExpr::new(Token::Identifier(name.to_string()))
}
#[cfg(test)]
pub fn new_ident_expr(name: &str) -> Expr {
    Expr::Ident(new_ident(name))
}

#[cfg(test)]
pub fn new_bool(value: bool) -> Expr {
    Expr::Bool(BoolLiteral::new(
        match value {
            true => Token::True,
            false => Token::False,
        },
        value,
    ))
}

#[cfg(test)]
pub fn new_int(value: i64) -> Expr {
    Expr::Int(IntLiteral::new(Token::Int(value.to_string()), value))
}

#[cfg(test)]
pub fn new_bang_prefix_expr(right: Expr) -> Expr {
    Expr::Prefix(PrefixExpr::new(Token::Bang, right))
}
#[cfg(test)]
pub fn new_minus_prefix_expr(right: Expr) -> Expr {
    Expr::Prefix(PrefixExpr::new(Token::Minus, right))
}

#[cfg(test)]
pub fn new_infix_expr(left: Expr, operator: Token, right: Expr) -> Expr {
    Expr::Infix(InfixExpr::new(operator, left, right))
}

#[cfg(test)]
pub fn new_let_stmt(ident: IdentExpr, expr: Expr) -> LetStmt {
    LetStmt::new(Token::Let, ident, expr)
}

#[cfg(test)]
pub fn new_expr_stmt(expr: Expr) -> Stmt {
    Stmt::Expression(ExprStmt::new(expr))
}

#[cfg(test)]
pub fn new_ret_stmt(expr: Expr) -> Stmt {
    Stmt::Return(ReturnStmt::new(Token::Return, expr))
}
