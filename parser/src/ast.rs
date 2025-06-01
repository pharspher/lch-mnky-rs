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

#[derive(Debug, PartialEq)]
pub enum Expr {
    Ident(IdentExpr),
    Int(IntLiteral),
    NoImpl,
}

#[derive(Debug, PartialEq)]
pub struct IdentExpr {
    token: Token,
}
impl IdentExpr {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

#[derive(Debug, PartialEq)]
pub struct IntLiteral {
    token: Token,
    value: i64,
}
impl IntLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}

pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
