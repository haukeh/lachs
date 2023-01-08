use std::collections::HashMap;

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
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
}

pub trait ExpressionVisitor<T> {
    fn visit_expr<'a>(&self, e: &'a Expr) -> T;
}

pub trait StatementVisitor<T> {
    fn visit_stmt(&mut self, e: &Stmt) -> T;
}

pub struct AstPrinter {}

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }

    pub fn print(&mut self, expr: &Expr) {
        println!("{}", self.visit_expr(expr));
    }

    pub fn string(&mut self, expr: &Expr) -> String {
        self.visit_expr(expr)
    }
}

impl ExpressionVisitor<String> for AstPrinter {
    fn visit_expr<'a>(&self, e: &'a Expr) -> String {
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
            Expr::Literal(value) => format!("{}", value),
            Expr::Variable(var) => {
                let value = var.literal.as_ref().unwrap_or(&LiteralValue::Nil);
                format!("var {} = {}", var.lexeme, value)
            }
        }
    }
}

struct Environment {
    values: HashMap<String, LiteralValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, val: LiteralValue) {
        self.values.insert(name, val);
    }

    pub fn get(&self, name: &str) -> &LiteralValue {
        self.values.get(name).expect(&format!("Undefined variable {}", name))
    }
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, program: &Vec<Stmt>) {
        for stmt in program {
            self.visit_stmt(stmt);
        }
    }
}

impl StatementVisitor<()> for Interpreter {
    fn visit_stmt(&mut self, e: &Stmt) -> () {
        match e {
            Stmt::Expression(expr) => {
                let _ = self.visit_expr(expr);
            }
            Stmt::Print(expr) => {
                let val = self.visit_expr(expr);
                println!("{val}");
            }
            Stmt::Var { name, initializer } => {
                let init = if let Some(value) = initializer {
                    self.visit_expr(value)
                } else {
                    LiteralValue::Nil
                };

                self.env.define(name.lexeme.clone(), init.clone());
            }
        }
    }
}

impl ExpressionVisitor<LiteralValue> for Interpreter {
    fn visit_expr(&self, expr: &Expr) -> LiteralValue {
        match expr {
            Expr::Unary { op, right } => {
                let right = self.visit_expr(right);
                match op.typ {
                    TokenType::Bang => {
                        if right.is_truthy() {
                            LiteralValue::False
                        } else {
                            LiteralValue::True
                        }
                    }
                    TokenType::Minus => {
                        if let LiteralValue::Number(num) = right {
                            return LiteralValue::Number(-num);
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
                        let lhs: f64 = left.into();
                        let rhs: f64 = right.into();
                        LiteralValue::Number(lhs - rhs)
                    }
                    TokenType::Slash => {
                        let lhs: f64 = left.into();
                        let rhs: f64 = right.into();
                        LiteralValue::Number(lhs / rhs)
                    }
                    TokenType::Star => {
                        let lhs: f64 = left.into();
                        let rhs: f64 = right.into();
                        LiteralValue::Number(lhs * rhs)
                    }
                    TokenType::Plus => left + right,
                    TokenType::Greater => {
                        if left > right {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::GreaterEqual => {
                        if left >= right {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::Less => {
                        if left < right {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::LessEqual => {
                        if left <= right {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::BangEqual => {
                        if left != right {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    TokenType::EqualEqual => {
                        if left == right {
                            LiteralValue::True
                        } else {
                            LiteralValue::False
                        }
                    }
                    _ => panic!("wrong token type"),
                }
            }
            Expr::Grouping { expr } => self.visit_expr(expr),
            Expr::Literal(lit) => lit.clone(),
            Expr::Variable(var) => {
                self.env.get(&var.lexeme).clone()
            }
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
