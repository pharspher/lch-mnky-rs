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
    Prefix(PrefixExpr),
    Infix(InfixExpr),
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
