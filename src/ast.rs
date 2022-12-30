use crate::scanner::{LiteralValue, Token};

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

pub trait Visitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> String;
}

struct AstPrinter {}

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }
}

impl Visitor<Expr> for AstPrinter {
    fn visit_expr(&mut self, e: &Expr) -> String {
        match e {
            Expr::Unary { op, right } => {
                let res = self.visit_expr(right);
                format!("({} {})", op.lexeme, res)
            }
            Expr::Binary {
                ref left,
                ref op,
                ref right,
            } => {
                let lhs = self.visit_expr(left);
                let rhs = self.visit_expr(right);
                format!("({} {} {})", op.lexeme, lhs, rhs)
            }
            Expr::Grouping { expr } => {
                let res = self.visit_expr(expr);
                format!("(group {})", res)
            }
            Expr::Literal(value) => {
                format!("{}", value)
            }
        }
    }
}

#[cfg(test)]
mod tests {
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

        assert_eq!(expr, "(* (- 123) (group 45.67))");
    }
}
