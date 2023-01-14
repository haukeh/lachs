use std::{cell::RefCell, rc::Rc};

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
            self.visit_stmt(stmt, self.env.clone());
        }
    }
}

impl StatementVisitor<()> for Interpreter {
    fn visit_stmt(&mut self, e: &Stmt, env: Rc<RefCell<Environment>>) -> () {
        match e {
            Stmt::Expression(expr) => {
                let _ = self.visit_expr(expr, env);
            }
            Stmt::Print(expr) => {
                let val = self.visit_expr(expr, env);
                println!("{val}");
            }
            Stmt::Var(name, initializer) => {
                let init = if let Some(value) = initializer {
                    self.visit_expr(value, Rc::clone(&env))
                } else {
                    LiteralValue::Nil
                };

                let mut e = (*env).borrow_mut();                
                e.define(name.lexeme.clone(), init.clone());
            }
            Stmt::Block(stmts) => {                
                let bor = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone( &self.env, ))));
                for stmt in stmts {
                    self.visit_stmt(stmt, Rc::clone(&bor));
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
            Expr::Variable(var) => (*env).borrow().get(&var.lexeme).clone(),
            Expr::Assign { name, value } => {
                let value = self.visit_expr(value, Rc::clone(&env));
                (*env).borrow_mut().assign(name, value.clone());
                value
            }
        }
    }
}
