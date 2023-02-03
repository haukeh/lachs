use std::{
    cell::RefCell,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
    io::{Write, self},
};

use crate::{
    ast::{Callable, Environment, Expr, Function, NativeFunction, Stmt, Value},
    scanner::{LiteralValue, TokenType},
};
use anyhow::anyhow;
use log::debug;

pub enum ReturnValue {
    Unit,
    Value(Value),
}

pub struct Interpreter<'a> {
    pub(crate) env: Rc<RefCell<Environment>>,
    out: &'a mut dyn Write,
}

impl <'a> Interpreter<'a> {
    pub fn new(out: &'a mut dyn Write) -> Self {
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

        let env = Environment::with_enclosing(Rc::new(RefCell::new(globals)));

        Interpreter {
            env: Rc::new(RefCell::new(env)),
            out: out,
        }
    }

    pub fn interpret(&mut self, program: &Vec<Stmt>) -> anyhow::Result<ReturnValue> {
        for stmt in program {
            let _ = self.statement(stmt)?;
        }

        Ok(ReturnValue::Unit)
    }

    fn expression(&mut self, expr: &Expr) -> anyhow::Result<Value> {
        match expr {
            Expr::Literal(lit) => match lit {
                LiteralValue::Number(n) => Ok(Value::Number(*n)),
                LiteralValue::String(s) => Ok(Value::String(s.clone())),
                LiteralValue::True => Ok(Value::Bool(true)),
                LiteralValue::False => Ok(Value::Bool(false)),
                LiteralValue::Nil => Ok(Value::Nil),
            },
            Expr::Unary { op, right } => {
                let right = self.expression(right)?;
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
                let lhs = self.expression(left)?;
                let rhs = self.expression(right)?;
                binay_operation(op.typ, lhs, rhs)
            }
            Expr::Grouping { expr } => self.expression(expr),
            Expr::Variable(var) => {
                let value = (*self.env).borrow().get(&var.lexeme);
                Ok(value)
            }
            Expr::Assign { name, value } => {
                let value = self.expression(value)?;
                let mut env = (*self.env).borrow_mut();
                env.assign(name, value.clone());
                Ok(value)
            }
            Expr::Logical { left, op, right } => {
                let left = self.expression(left)?;
                match op.typ {
                    TokenType::Or if left.is_truthy() => Ok(left),
                    _ if !left.is_truthy() => Ok(left),
                    _ => self.expression(right),
                }
            }
            Expr::Call {
                callee,
                paren: _,
                args,
            } => {
                let callee = self.expression(callee)?;

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
                    .flat_map(|arg| self.expression(arg))
                    .collect::<Vec<Value>>();

                callee.call(self, &args)
            }
        }
    }

    pub(crate) fn statement(&mut self, e: &Stmt) -> anyhow::Result<ReturnValue> {
        match e {
            Stmt::Expression(expr) => {
                let _ = self.expression(expr)?;
                Ok(ReturnValue::Unit)
            }
            Stmt::Print(expr) => {
                let value = self.expression(expr)?;
                writeln!(&mut self.out, "{}", value);
                Ok(ReturnValue::Unit)
            }
            Stmt::Var(name, initializer) => {
                let init = if let Some(value) = initializer {
                    self.expression(value)?
                } else {
                    Value::Nil
                };

                debug!("inserting {}={} into env {:?}", name.lexeme, init, self.env);

                let mut e = (*self.env).borrow_mut();
                e.define(name.lexeme.clone(), init);
                Ok(ReturnValue::Unit)
            }
            Stmt::Block(stmts) => {
                let env = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
                    &self.env,
                ))));
                self.execute_block(stmts, env)
            }
            Stmt::If { cond, then, elze } => {
                if self.expression(cond)?.is_truthy() {
                    if let rv @ ReturnValue::Value(_) = self.statement(then)? {
                        return Ok(rv);
                    }
                } else if let Some(else_stmt) = elze {
                    if let rv @ ReturnValue::Value(_) = self.statement(else_stmt)? {
                        return Ok(rv);
                    }
                }
                Ok(ReturnValue::Unit)
            }
            Stmt::While(cond, body) => {
                while self.expression(cond)?.is_truthy() {
                    if let rv @ ReturnValue::Value(_) = self.statement(body)? {
                        return Ok(rv);
                    }
                }
                Ok(ReturnValue::Unit)
            }
            Stmt::Function(func) => {
                let f = Value::Func(Function {
                    declaration: func.clone(),
                    closure: Rc::clone(&self.env),
                });

                (*self.env).borrow_mut().define(func.name.lexeme.clone(), f);

                Ok(ReturnValue::Unit)
            }

            Stmt::Return(_, expr) => {
                let v = self.expression(expr)?;
                Ok(ReturnValue::Value(v))
            }
        }
    }

    pub(crate) fn execute_block(
        &mut self,
        stmts: &Vec<Stmt>,
        env: Rc<RefCell<Environment>>,
    ) -> anyhow::Result<ReturnValue> {
        let prev_env = Rc::clone(&self.env);
        self.env = Rc::clone(&env);

        let mut result = Ok(ReturnValue::Unit);
        for stmt in stmts {
            match self.statement(stmt) {
                ret @ Ok(ReturnValue::Value(_)) => {
                    result = ret;
                    break;
                }
                err @ Err(_) => {
                    result = err;
                    break;
                }
                _ => { /* Unit return value */ }
            }
        }

        self.env = prev_env;
        result
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