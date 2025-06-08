use crate::ast::{BoolLiteral, Expr, IdentExpr, IntLiteral};
use lexer::token::Token;

#[cfg(test)]
pub fn new_ident(name: &str) -> Expr {
    Expr::Ident(IdentExpr::new(Token::Identifier(name.to_string())))
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
