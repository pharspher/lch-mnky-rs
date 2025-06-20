use derive_more::with_trait::Display;
use lexer::token::Token;
use std::fmt;

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

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Let(LetStmt),
    Return(ReturnStmt),
    Expression(ExprStmt),
}
impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let(let_stmt) => write!(f, "{}", let_stmt),
            Stmt::Return(return_stmt) => write!(f, "{}", return_stmt),
            Stmt::Expression(expr_stmt) => write!(f, "{}", expr_stmt),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
            write!(f, "Unexpected token {:?} in LetStmt", self.name.token)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}
impl ExprStmt {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
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
    Bool(BoolLiteral),
    If(IfExpr),
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ident) => write!(f, "{}", ident),
            Expr::Int(int) => write!(f, "{}", int),
            Expr::Prefix(prefix) => write!(f, "{}", prefix),
            Expr::Infix(infix) => write!(f, "{}", infix),
            Expr::Bool(bool) => write!(f, "{}", bool),
            Expr::If(if_expr) => write!(f, "{}", if_expr),
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
            write!(f, "Unexpected token {:?} in IdentExpr", self.token)
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
            write!(f, "Unexpected token {:?} in IntLiteral", self.token)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolLiteral {
    pub token: Token,
    pub value: bool,
}
impl BoolLiteral {
    pub fn new(token: Token, value: bool) -> Self {
        Self { token, value }
    }
}
impl fmt::Display for BoolLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.token {
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            _ => {
                write!(f, "Unexpected token {:?} in BoolLiteral", self.token)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpr {
    pub token: Token,
    pub condition: Box<Expr>,
    pub consequence: Box<BlockStmt>,
    pub alternative: Option<Box<BlockStmt>>,
}
impl IfExpr {
    pub fn new(condition: Expr, consequence: BlockStmt, alternative: Option<BlockStmt>) -> Self {
        Self {
            token: Token::If,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative.map(Box::new),
        }
    }
}
impl fmt::Display for IfExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut result = format!("if ({}) {}", self.condition, self.consequence.as_ref());
        if let Some(alternative) = &self.alternative {
            result.push_str(&format!(" else {}", alternative.as_ref()));
        }
        write!(f, "{}", result)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}
impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}
impl fmt::Display for BlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stmts_str: Vec<String> = self.stmts.iter().map(|s| s.to_string()).collect();
        write!(f, "{{{}}}", stmts_str.join("\n"))
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
    pub token: Token,
    pub left_expr: Box<Expr>,
    pub right_expr: Box<Expr>,
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

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Display)]
pub enum Precedence {
    #[display("Lowest")]
    Lowest,
    #[display("Equals")]
    Equals,
    #[display("LessGreater")]
    LessGreater,
    #[display("Sum")]
    Sum,
    #[display("Product")]
    Product,
    #[display("Prefix")]
    Prefix,
    #[display("Call")]
    Call,
}

#[cfg(test)]
mod display_tests {
    use lexer::token::Token;

    use crate::ast::{BlockStmt, Expr, ExprStmt, IfExpr, Stmt};
    use crate::init_logger;
    use crate::test_utils::{
        new_bang_prefix_expr, new_bool, new_ident_expr, new_infix_expr, new_int,
        new_minus_prefix_expr,
    };

    #[test]
    fn test_expr_display() {
        init_logger();

        // TODO: IntLiteral can only accept Token::Int, try make it explicit
        let int_expr = new_int(5);
        assert_eq!(int_expr.to_string(), "5");

        // TODO: IdentExpr can only accept Token::Identifier, try make it explicit
        let ident_expr = new_ident_expr("x");
        assert_eq!(ident_expr.to_string(), "x");

        // TODO: PrefixExpr can only accept Token::Bang or Token::Minus, try make it explicit
        let bang_prefix_expr = new_bang_prefix_expr(int_expr.clone());
        assert_eq!(bang_prefix_expr.to_string(), "!(5)");

        let minus_prefix_expr = new_minus_prefix_expr(ident_expr.clone());
        assert_eq!(minus_prefix_expr.to_string(), "-(x)");

        // TODO: InfixExpr can only accept Token::Plus, Token::Minus, etc., try make it explicit
        let infix_expr = new_infix_expr(
            bang_prefix_expr.clone(),
            Token::Plus,
            minus_prefix_expr.clone(),
        );
        assert_eq!(infix_expr.to_string(), "(!(5)) + (-(x))");

        let infix_expr = new_infix_expr(infix_expr.clone(), Token::Asterisk, infix_expr.clone());
        assert_eq!(
            infix_expr.to_string(),
            "((!(5)) + (-(x))) * ((!(5)) + (-(x)))"
        );

        let true_expr = new_bool(true);
        assert_eq!(true_expr.to_string(), "true");

        let false_expr = new_bool(false);
        assert_eq!(false_expr.to_string(), "false");

        let if_expr = Expr::If(IfExpr::new(
            new_infix_expr(new_ident_expr("x"), Token::LT, new_ident_expr("y")),
            BlockStmt::new(vec![Stmt::Expression(ExprStmt::new(int_expr.clone()))]),
            Some(BlockStmt::new(vec![Stmt::Expression(ExprStmt::new(
                ident_expr.clone(),
            ))])),
        ));
        assert_eq!(if_expr.to_string(), "if ((x) < (y)) {5;} else {x;}");
    }
}
