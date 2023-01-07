use std::{borrow::Cow, ops::Deref};

use crate::scanner::{LiteralValue, Token, TokenType};

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
    Variable(Token),
}

pub enum Stmt {
    Expression(Expr), 
    Print(Expr),
    Var(Token, Expr),
}

pub trait ExpressionVisitor<T: Clone> {
    fn visit_expr<'a>(&self, e: &'a Expr) -> Cow<'a, T>;    
}

pub trait StatementVisitor<T> {
    fn visit_stmt(&self, e: &Stmt) -> T;
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

impl ExpressionVisitor<String> for AstPrinter {
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

impl Interpreter {

    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(&self, program: &Vec<Stmt>) {
        for stmt in program {
            self.visit_stmt(stmt);
        }
    }
}

impl StatementVisitor<()> for Interpreter {
    fn visit_stmt(&self, e: &Stmt) -> () {
        match e {
            Stmt::Expression(expr) => {
                let _ = self.visit_expr(expr);
            },
            Stmt::Print(expr) => {
                let val = self.visit_expr(expr);
                println!("{val}");
            },
            Stmt::Var(_, _) => todo!(),
        }
    }
}

impl ExpressionVisitor<LiteralValue> for Interpreter {
    fn visit_expr<'a>(&self, e: &'a Expr) -> Cow<'a, LiteralValue> {
        match e {
            Expr::Unary { op, right } => {
                let right = self.visit_expr(right);
                match op.typ {
                    TokenType::Bang => {
                        let b = if right.is_truthy() {
                            LiteralValue::False
                        } else {
                            LiteralValue::True
                        };
                        Cow::Owned(b)
                    }
                    TokenType::Minus => {
                        if let LiteralValue::Number(num) = *right {
                            return Cow::Owned(LiteralValue::Number(-num));
                        }
                        panic!("minus only works for number")
                    }
                    _ => panic!("wrong token type"),
                }
            }
            Expr::Binary { left, op, right } => {
                let left = self.visit_expr(left);
                let right = self.visit_expr(right);
                
                match op.typ {
                    TokenType::Minus => {
                        let lhs: f64 = left.deref().into();
                        let rhs: f64 = right.deref().into();
                        Cow::Owned(LiteralValue::Number(lhs - rhs))
                    },
                    TokenType::Slash => {
                        let lhs: f64 = left.deref().into();
                        let rhs: f64 = right.deref().into();
                        Cow::Owned(LiteralValue::Number(lhs / rhs))
                    },
                    TokenType::Star => {
                        let lhs: f64 = left.deref().into();
                        let rhs: f64 = right.deref().into();
                        Cow::Owned(LiteralValue::Number(lhs * rhs))
                    },
                    TokenType::Plus => {
                        Cow::Owned(left.deref() + right.deref())
                    },
                    TokenType::Greater => {
                        if left.deref() > right.deref() { 
                            Cow::Owned(LiteralValue::True)
                        } else {
                            Cow::Owned(LiteralValue::False)
                        }
                    },
                    TokenType::GreaterEqual => {
                        if left.deref() >= right.deref() { 
                            Cow::Owned(LiteralValue::True)
                        } else {
                            Cow::Owned(LiteralValue::False)
                        }
                    },
                    TokenType::Less => {
                        if left.deref() < right.deref() { 
                            Cow::Owned(LiteralValue::True)
                        } else {
                            Cow::Owned(LiteralValue::False)
                        }
                    },
                    TokenType::LessEqual => {
                        if left.deref() <= right.deref() { 
                            Cow::Owned(LiteralValue::True)
                        } else {
                            Cow::Owned(LiteralValue::False)
                        }
                    },
                    TokenType::BangEqual => {
                        if left.deref() != right.deref() { 
                            Cow::Owned(LiteralValue::True)
                        } else {
                            Cow::Owned(LiteralValue::False)
                        }
                    },
                    TokenType::EqualEqual => {
                        if left.deref() == right.deref() { 
                            Cow::Owned(LiteralValue::True)
                        } else {
                            Cow::Owned(LiteralValue::False)
                        }
                    },
                    _ => panic!("wrong token type")
                }
            },
            Expr::Grouping { expr } => self.visit_expr(expr),
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
