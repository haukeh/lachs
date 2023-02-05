use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    io::Write,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::{Callable, Environment, Expr, ExprID, Function, NativeFunction, Stmt, Value},
    scanner::{LiteralValue, Token, TokenType},
};
use anyhow::anyhow;
use log::debug;

pub enum ReturnValue {
    Unit,
    Value(Value),
}

pub struct Interpreter<'a> {
    pub(crate) env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    locals: HashMap<ExprID, usize>,
    out: &'a mut dyn Write,
}

impl<'a> Interpreter<'a> {
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

        let globals = Rc::new(RefCell::new(globals));
        let env = Rc::clone(&globals);

        Interpreter {
            globals,
            env,
            locals: HashMap::new(),
            out,
        }
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.id(), depth);
    }

    pub fn interpret(&mut self, program: &Vec<Stmt>) -> anyhow::Result<ReturnValue> {
        for stmt in program {
            let _ = self.statement(stmt)?;
        }

        Ok(ReturnValue::Unit)
    }

    fn lookup_variable(&self, name: &Token, expr: &Expr) -> Value {
        match self.locals.get(&expr.id()) {
            Some(distance) => {            
                self.env.borrow().get_at(*distance, &name.lexeme)
            }
            None => {             
                self.globals.borrow().get(&name.lexeme)
            }
        }
    }

    fn expression(&mut self, expr: &Expr) -> anyhow::Result<Value> {
        match expr {
            Expr::Literal(_, lit) => match lit {
                LiteralValue::Number(n) => Ok(Value::Number(*n)),
                LiteralValue::String(s) => Ok(Value::String(s.clone())),
                LiteralValue::True => Ok(Value::Bool(true)),
                LiteralValue::False => Ok(Value::Bool(false)),
                LiteralValue::Nil => Ok(Value::Nil),
            },
            Expr::Unary { id: _, op, right } => {
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
            Expr::Binary {
                id: _,
                left,
                op,
                right,
            } => {
                let lhs = self.expression(left)?;
                let rhs = self.expression(right)?;
                binay_operation(op.typ, lhs, rhs)
            }
            Expr::Grouping { id: _, expr } => self.expression(expr),
            Expr::Variable(_, name) => Ok(self.lookup_variable(name, expr)),
            Expr::Assign { id: _, name, value } => {
                let value = self.expression(value)?;
                if let Some(distance) = self.locals.get(&expr.id()) {
                    self.env
                        .borrow_mut()
                        .assign_at(*distance, name, value.clone());
                } else {
                    self.globals.borrow_mut().assign(name, value.clone());
                }
                Ok(value)
            }
            Expr::Logical {
                id: _,
                left,
                op,
                right,
            } => {
                let left = self.expression(left)?;
                match op.typ {
                    TokenType::Or if left.is_truthy() => Ok(left),
                    _ if !left.is_truthy() => Ok(left),
                    _ => self.expression(right),
                }
            }
            Expr::Call {
                id: _,
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
                writeln!(&mut self.out, "{}", value)?;
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
