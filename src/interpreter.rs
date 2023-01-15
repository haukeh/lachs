use std::{cell::RefCell, rc::Rc};

use log::debug;

use crate::{
    ast::{Environment, Expr, ExpressionVisitor, StatementVisitor, Stmt},
    scanner::{LiteralValue, TokenType},
};

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn interpret(&mut self, program: &Vec<Stmt>) {
        for stmt in program {
            self.visit_stmt(stmt, Rc::clone(&self.env));
        }
    }
}

impl StatementVisitor<()> for Interpreter {
    fn visit_stmt(&mut self, e: &Stmt, env: Rc<RefCell<Environment>>) {
        match e {
            Stmt::Expression(expr) => {
                let _ = self.visit_expr(expr, Rc::clone(&env));
            }
            Stmt::Print(expr) => {
                let val = self.visit_expr(expr, Rc::clone(&env));
                println!("{val}");
            }
            Stmt::Var(name, initializer) => {
                let init = if let Some(value) = initializer {
                    self.visit_expr(value, Rc::clone(&env))
                } else {
                    LiteralValue::Nil
                };

                debug!("inserting {}={} into env {:?}", name.lexeme, init, env);

                let mut e = (*env).borrow_mut();
                e.define(name.lexeme.clone(), init);
            }
            Stmt::Block(stmts) => {
                let subscope_env =
                    Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(&env))));
                for stmt in stmts {
                    self.visit_stmt(stmt, Rc::clone(&subscope_env));
                }
            }
            Stmt::If { cond, then, elze } => {
                if self.visit_expr(cond, Rc::clone(&env)).is_truthy() {
                    self.visit_stmt(then, Rc::clone(&env));
                } else if let Some(else_stmt) = elze {
                    self.visit_stmt(else_stmt, Rc::clone(&env));
                }
            }
            Stmt::While(cond, body) => {
                while self.visit_expr(cond, Rc::clone(&env)).is_truthy() {
                    self.visit_stmt(body, Rc::clone(&env));
                }
            }
        }
    }
}

impl ExpressionVisitor<LiteralValue> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr, env: Rc<RefCell<Environment>>) -> LiteralValue {
        match expr {
            Expr::Unary { op, right } => {
                let right = self.visit_expr(right, Rc::clone(&env));
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
                let left = self.visit_expr(left, Rc::clone(&env));
                let right = self.visit_expr(right, Rc::clone(&env));

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
            Expr::Grouping { expr } => self.visit_expr(expr, Rc::clone(&env)),
            Expr::Literal(lit) => lit.clone(),
            Expr::Variable(var) => (*env).borrow().get(&var.lexeme),
            Expr::Assign { name, value } => {
                let value = self.visit_expr(value, Rc::clone(&env));
                let mut env = (*env).borrow_mut();
                env.assign(name, value.clone());
                value
            }
            Expr::Logical { left, op, right } => {
                let left = self.visit_expr(left, Rc::clone(&env));
                match op.typ {
                    TokenType::Or if left.is_truthy() => left,
                    _ if !left.is_truthy() => left,
                    _ => self.visit_expr(right, Rc::clone(&env)),
                }
            }
        }
    }
}
