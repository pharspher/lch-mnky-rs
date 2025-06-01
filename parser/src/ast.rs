use std::{any, fmt};

use downcast_rs::Downcast;

use lexer::token::Token;

pub trait Statement: fmt::Debug + any::Any + Downcast {}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl LetStatement {
    pub fn new(token: Token, identifier: Identifier, expression: Expression) -> Self {
        Self {
            token,
            name: identifier,
            value: expression
        }
    }
}

impl Statement for LetStatement {}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    token: Token,
    value: Expression,
}

impl ReturnStatement {
    pub fn new(token: Token, expression: Expression) -> Self {
        Self {
            token,
            value: expression
        }
    }
}

impl Statement for ReturnStatement {}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    token: Token,
    expression: Expression,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> Self {
        Self {
            token,
            expression
        }
    }
}

impl Statement for ExpressionStatement {}

#[derive(Debug, PartialEq)]
pub struct Expression {}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Default for Program {
    fn default() -> Self {
        Program::new()
    }
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}
