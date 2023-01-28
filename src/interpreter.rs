use std::{
    cell::RefCell,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::{
        Callable, Environment, Expr, Function, NativeFunction, Stmt, Value, ValueConversionError,
    },
    scanner::{LiteralValue, TokenType},
};
use anyhow::anyhow;
use log::debug;
use thiserror::Error;
#[derive(Error, Debug)]
pub enum IError {
    #[error("Invalid operands in binary operation")]
    InvalidOperands,
    #[error("Invalid operator in binary operation")]
    InvalidOperator,
    #[error(transparent)]
    ConversionError(#[from] ValueConversionError),
}

pub struct Interpreter {
    pub(crate) env: Rc<RefCell<Environment>>,
    pub(crate) globals: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Environment::new();
        globals.define(
            "clock".to_string(),
            Value::NativeFunc(NativeFunction {
                arity: 0,
                f: |_, _| {
                    let duration_since_epoch =
                        SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                    Ok(Value::Number(duration_since_epoch.as_secs() as f64))
                },
            }),
        );

        let globals = Rc::new(RefCell::new(globals));

        Interpreter {
            env: Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
                &globals,
            )))),
            globals,
        }
    }

    pub fn interpret(&mut self, program: &Vec<Stmt>) -> anyhow::Result<()> {
        for stmt in program {
            self.statement(stmt, Rc::clone(&self.env))?;
        }

        Ok(())
    }

    fn expression(&mut self, expr: &Expr, env: Rc<RefCell<Environment>>) -> anyhow::Result<Value> {
        match expr {
            Expr::Literal(lit) => match lit {
                LiteralValue::Number(n) => Ok(Value::Number(*n)),
                LiteralValue::String(s) => Ok(Value::String(s.clone())),
                LiteralValue::True => Ok(Value::Bool(true)),
                LiteralValue::False => Ok(Value::Bool(false)),
                LiteralValue::Nil => Ok(Value::Nil),
            },
            Expr::Unary { op, right } => {
                let right = self.expression(right, Rc::clone(&env))?;
                match op.typ {
                    TokenType::Bang => Ok(Value::Bool(!right.is_truthy())),
                    TokenType::Minus => {
                        if let Value::Number(num) = right {
                            return Ok(Value::Number(-num));
                        }
                        Err(anyhow!("minus only works for number"))
                    }
                    _ => Err(anyhow!("wrong token type")),
                }
            }
            Expr::Binary { left, op, right } => {
                let lhs = self.expression(left, Rc::clone(&env))?;
                let rhs = self.expression(right, Rc::clone(&env))?;
                binay_operation(op.typ, lhs, rhs)
            }
            Expr::Grouping { expr } => self.expression(expr, Rc::clone(&env)),
            Expr::Variable(var) => {
                let value = (*env).borrow().get(&var.lexeme);
                Ok(value)
            }
            Expr::Assign { name, value } => {
                let value = self.expression(value, Rc::clone(&env))?;
                let mut env = (*env).borrow_mut();
                env.assign(name, value.clone());
                Ok(value)
            }
            Expr::Logical { left, op, right } => {
                let left = self.expression(left, Rc::clone(&env))?;
                match op.typ {
                    TokenType::Or if left.is_truthy() => Ok(left),
                    _ if !left.is_truthy() => Ok(left),
                    _ => self.expression(right, Rc::clone(&env)),
                }
            }
            Expr::Call {
                callee,
                paren: _,
                args,
            } => {
                let callee = self.expression(callee, Rc::clone(&self.env))?;

                let callee: &dyn Callable = match &callee {
                    Value::NativeFunc(f) => f,
                    Value::Func(f) => f,
                    _ => return Err(anyhow!("Callee {:?} is not a function or class", callee)),
                };

                if args.len() != callee.arity() {
                    return Err(anyhow!(
                        "expected {} arguments, but got {}",
                        callee.arity(),
                        args.len()
                    ));
                }

                let args = args
                    .iter()
                    .flat_map(|arg| self.expression(arg, Rc::clone(&self.env)))
                    .collect::<Vec<Value>>();

                callee.call(self, &args)
            }
        }
    }

    pub(crate) fn statement(
        &mut self,
        e: &Stmt,
        env: Rc<RefCell<Environment>>,
    ) -> anyhow::Result<()> {
        match e {
            Stmt::Expression(expr) => {
                let _ = self.expression(expr, Rc::clone(&env))?;
                Ok(())
            }
            Stmt::Print(expr) => {
                let value = self.expression(expr, Rc::clone(&env))?;
                println!("{}", value);
                Ok(())
            }
            Stmt::Var(name, initializer) => {
                let init = if let Some(value) = initializer {
                    self.expression(value, Rc::clone(&env))?
                } else {
                    Value::Nil
                };

                debug!("inserting {}={} into env {:?}", name.lexeme, init, env);

                let mut e = (*env).borrow_mut();
                e.define(name.lexeme.clone(), init);
                Ok(())
            }
            Stmt::Block(stmts) => self.execute_block(stmts, env),
            Stmt::If { cond, then, elze } => {
                if self.expression(cond, Rc::clone(&env))?.is_truthy() {
                    self.statement(then, Rc::clone(&env))?;
                } else if let Some(else_stmt) = elze {
                    self.statement(else_stmt, Rc::clone(&env))?;
                }
                Ok(())
            }
            Stmt::While(cond, body) => {
                while self.expression(cond, Rc::clone(&env))?.is_truthy() {
                    self.statement(body, Rc::clone(&env))?;
                }
                Ok(())
            }
            Stmt::Function(func) => {
                let f = Value::Func(Function {
                    declaration: func.clone(),
                });

                (*env).borrow_mut().define(func.name.lexeme.clone(), f);

                Ok(())
            }
        }
    }

    pub(crate) fn execute_block(
        &mut self,
        stmts: &Vec<Stmt>,
        env: Rc<RefCell<Environment>>,
    ) -> anyhow::Result<()> {
        let subscope_env = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(&env))));
        for stmt in stmts {
            self.statement(stmt, Rc::clone(&subscope_env))?;
        }
        Ok(())
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

fn binay_operation(op: TokenType, lhs: Value, rhs: Value) -> anyhow::Result<Value> {
    match op {
        TokenType::Minus => lhs - rhs,
        TokenType::Star => lhs * rhs,
        TokenType::Plus => lhs + rhs,
        TokenType::Greater => Ok(Value::Bool(lhs > rhs)),
        TokenType::GreaterEqual => Ok(Value::Bool(lhs >= rhs)),
        TokenType::Less => Ok(Value::Bool(lhs < rhs)),
        TokenType::LessEqual => Ok(Value::Bool(lhs <= rhs)),
        TokenType::BangEqual => Ok(Value::Bool(lhs != rhs)),
        TokenType::EqualEqual => Ok(Value::Bool(lhs == rhs)),
        _ => Err(anyhow!(
            "invalid operator {:?} for binary operation between {} and {}",
            op,
            lhs,
            rhs
        )),
    }
}
