use std::borrow::Cow;

use crate::scanner::{LiteralValue, Token};

#[derive(Debug)]
pub enum Expr {
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Literal(LiteralValue),
}

pub trait Visitor<T: Clone> {
    fn visit_expr<'a>(&self, e: &'a Expr) -> Cow<'a, T>;
}

pub struct AstPrinter {}

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }

    pub fn print(&mut self, expr: &Expr) {
        println!("{}", self.visit_expr(expr));
    }

    pub fn string<'a>(&mut self, expr: &'a Expr) -> String {
        self.visit_expr(expr).into_owned()
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_expr<'a>(&self, e: &'a Expr) -> Cow<'a, String> {
        match e {
            Expr::Unary { op, right } => {
                let res = self.visit_expr(right);
                Cow::Owned(format!("({} {})", op.lexeme, res))
            }
            Expr::Binary {
                ref left,
                ref op,
                ref right,
            } => {
                let lhs = self.visit_expr(left);
                let rhs = self.visit_expr(right);
                Cow::Owned(format!("({} {} {})", op.lexeme, lhs, rhs))
            }
            Expr::Grouping { expr } => {
                let res = self.visit_expr(expr);
                Cow::Owned(format!("(group {})", res))
            }
            Expr::Literal(value) => Cow::Owned(format!("{}", value)),
        }
    }
}
pub struct Interpreter {}

impl Visitor<LiteralValue> for Interpreter {
    fn visit_expr<'a>(&self, e: &'a Expr) -> Cow<'a, LiteralValue> {
        match e {
            Expr::Unary { op, right } => todo!(),
            Expr::Binary { left, op, right } => todo!(),
            Expr::Grouping { expr } => todo!(),
            Expr::Literal(lit) => Cow::Borrowed(lit),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use crate::scanner::TokenType;

    use super::*;

    #[test]
    fn test_ast_printer() {
        let expr = Expr::Binary {
            left: Box::new(Expr::Unary {
                op: Token::new(TokenType::Minus, "-".to_string(), None, 1),
                right: Box::new(Expr::Literal(LiteralValue::Number(123.0))),
            }),
            op: Token::new(TokenType::Star, "*".to_string(), None, 1),
            right: Box::new(Expr::Grouping {
                expr: Box::new(Expr::Literal(LiteralValue::Number(45.67))),
            }),
        };

        let expr = AstPrinter::new().visit_expr(&expr);

        assert_eq!(expr.deref(), "(* (- 123) (group 45.67))");
    }
}
