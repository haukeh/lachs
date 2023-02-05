use anyhow::anyhow;
use core::fmt;
use log::debug;

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display, write},
    ops::{Add, Mul, Sub},
    rc::Rc,
};
use thiserror::Error;

use crate::{
    interpreter::{Interpreter, ReturnValue},
    scanner::{LiteralValue, Token},
};

pub trait Callable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value>;
}

#[derive(Clone)]
pub struct Function {
    pub declaration: FunctionDecl,
    pub closure: Rc<RefCell<Environment>>,
}

impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.declaration.name.lexeme)
            .field(
                "params",
                &self
                    .declaration
                    .params
                    .iter()
                    .map(|p| &p.lexeme)
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct ClassInstance {
    class: Class,
}

impl ClassInstance {
    pub fn new(class: Class) -> Self {
        ClassInstance { class }
    }
}

impl Display for ClassInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.class.name)
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
}

impl Class {
    pub fn new(name: String) -> Self {
        Class { name }
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _interpreter: &mut Interpreter, _args: &[Value]) -> anyhow::Result<Value> {
        let instance = ClassInstance::new(self.clone());
        Ok(Value::ClassInstance(instance))
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    pub arity: usize,
    pub f: fn(&mut Interpreter, &[Value]) -> anyhow::Result<Value>,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Function@{:p}", self)
    }
}

impl Callable for NativeFunction {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        (self.f)(interpreter, args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}

impl Callable for Function {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> anyhow::Result<Value> {
        let FunctionDecl {
            name: _,
            params,
            body,
        } = &self.declaration;

        let mut env = Environment::with_enclosing(Rc::clone(&self.closure));

        for i in 0..params.len() {
            env.define(params[i].lexeme.clone(), args[i].clone());
        }

        let res = match interpreter.execute_block(body, Rc::new(RefCell::new(env)))? {
            ReturnValue::Value(v) => v,
            ReturnValue::Unit => Value::Nil,
        };

        Ok(res)
    }

    fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
    NativeFunc(NativeFunction),
    Func(Function),
    Class(Class),
    ClassInstance(ClassInstance),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Nil)
    }
}

impl Add<Value> for Value {
    type Output = anyhow::Result<Value>;

    fn add(self, rhs: Value) -> anyhow::Result<Value> {
        if let (Value::String(lhs), Value::String(rhs)) = (&self, &rhs) {
            return Ok(Value::String(format!("{}{}", lhs, rhs)));
        }
        if let (Value::Number(lhs), Value::Number(rhs)) = (&self, &rhs) {
            return Ok(Value::Number(lhs + rhs));
        }
        Err(anyhow!("Incompatible operands for add {} + {}", self, rhs))
    }
}

impl Sub<Value> for Value {
    type Output = anyhow::Result<Value>;

    fn sub(self, rhs: Value) -> anyhow::Result<Value> {
        if let (Value::Number(lhs), Value::Number(rhs)) = (&self, &rhs) {
            return Ok(Value::Number(lhs - rhs));
        }
        Err(anyhow!("Incompatible operands for add {} + {}", self, rhs))
    }
}

impl Mul<Value> for Value {
    type Output = anyhow::Result<Value>;

    fn mul(self, rhs: Value) -> anyhow::Result<Value> {
        if let (Value::Number(lhs), Value::Number(rhs)) = (&self, &rhs) {
            return Ok(Value::Number(lhs * rhs));
        }
        Err(anyhow!(
            "Incompatible operands for multiplication {} * {}",
            self,
            rhs
        ))
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => false, // TODO: comparable funcs?
        }
    }
}

impl PartialOrd<Value> for Value {
    fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(r0),
            (Self::String(l0), Self::String(r0)) => l0.partial_cmp(r0),
            (Self::Bool(l0), Self::Bool(r0)) => l0.partial_cmp(r0),
            _ => None,
        }
    }
}

#[derive(Error, Debug)]
pub enum ValueConversionError {
    #[error("Value cannot be cast to a number")]
    NotANumber,
    #[error("Value cannot be cast to a string")]
    NotAString,
}

// Can't use TryFrom because<Value> because of conflicting blanket impl in core:
// = note: conflicting implementation in crate `core`:
// - impl<T, U> TryFrom<U> for T
// where U: Into<T>;
impl TryFrom<String> for Value {
    type Error = ValueConversionError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(Value::String(value))
    }
}

impl From<Value> for String {
    fn from(value: Value) -> Self {
        match value {
            Value::Number(n) => n.to_string(),
            Value::String(s) => s,
            Value::Bool(b) => b.to_string(),
            Value::Nil => "nil".to_string(),
            Value::Func(_) => todo!(),
            Value::NativeFunc(_) => todo!(),
            Value::Class(_) => todo!(),
            Value::ClassInstance(_) => todo!(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Func(_) => write!(f, "Function"),
            Value::NativeFunc(_) => write!(f, "NativeFunction"),
            Value::Class(Class { name }) => write!(f, "{name}"),
            Value::ClassInstance(ClassInstance { class }) => write!(f, "{} instance", class.name),
        }
    }
}

pub type ExprID = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Unary {
        id: ExprID,
        op: Token,
        right: Box<Expr>,
    },
    Logical {
        id: ExprID,
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Binary {
        id: ExprID,
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Call {
        id: ExprID,
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
    Assign {
        id: ExprID,
        name: Token,
        value: Box<Expr>,
    },
    Grouping {
        id: ExprID,
        expr: Box<Expr>,
    },
    Literal(ExprID, LiteralValue),
    Variable(ExprID, Token),
}

impl Expr {
    pub fn id(&self) -> ExprID {
        match self {
            Expr::Unary {
                id,
                op: _,
                right: _,
            } => *id,
            Expr::Logical {
                id,
                left: _,
                op: _,
                right: _,
            } => *id,
            Expr::Binary {
                id,
                left: _,
                op: _,
                right: _,
            } => *id,
            Expr::Call {
                id,
                callee: _,
                paren: _,
                args: _,
            } => *id,
            Expr::Assign {
                id,
                name: _,
                value: _,
            } => *id,
            Expr::Grouping { id, expr: _ } => *id,
            Expr::Literal(id, _) => *id,
            Expr::Variable(id, _) => *id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Expr),
    If {
        cond: Expr,
        then: Box<Stmt>,
        elze: Option<Box<Stmt>>,
    },
    Print(Expr),
    Return(Token, Box<Expr>),
    Var(Token, Option<Expr>),
    While(Expr, Box<Stmt>),
    Block(Vec<Stmt>),
    Function(FunctionDecl),
    Class(Token, Vec<FunctionDecl>),
}

#[derive(Debug, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_enclosing(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(parent),
        }
    }

    pub fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    pub fn assign(&mut self, name: &Token, val: Value) {
        debug!("assignment {:?}={} [env: {:?}", name.lexeme, val, self);

        if let Some(local) = self.values.get_mut(&name.lexeme) {
            *local = val;
        } else if let Some(parent_env) = self.enclosing.as_ref() {
            parent_env.borrow_mut().assign(name, val);
        } else {
            panic!("Assign to undefined variable {}", name.lexeme)
        }
    }

    pub fn assign_at(&mut self, distance: usize, name: &Token, value: Value) {
        if distance == 0 {
            self.values.insert(name.lexeme.clone(), value);
            return;
        }

        let mut next_env = self.enclosing.clone().unwrap();
        for _ in 1..distance {
            let tmp = &(next_env.borrow().enclosing.clone().unwrap());
            next_env = Rc::clone(tmp);
        }

        let mut env = next_env.borrow_mut();

        env.values.insert(name.lexeme.clone(), value);
    }

    pub fn get(&self, name: &str) -> Value {
        debug!("looking for {} in {:?}", name, self.values);

        if let Some(local) = self.values.get(name) {
            debug!("found: {}", local);
            return local.clone();
        }

        if let Some(parent_env) = self.enclosing.as_ref() {
            let p = parent_env.borrow();
            return p.get(name);
        }

        panic!("Undefined variable {}", name);
    }

    pub fn get_at(&self, distance: usize, name: &str) -> Value {
        debug!("Values at distance 0: {:?}", self.values);
        if distance == 0 {
            return self
                .values
                .get(name)
                .expect("Variable not found at expected depth")
                .clone();
        }

        let mut next_env = self.enclosing.clone().unwrap();
        for _ in 1..distance {
            let tmp = &(next_env.borrow().enclosing.clone().unwrap());
            next_env = Rc::clone(tmp);
        }

        let env = next_env.borrow();

        debug!("Values at distance {}: {:?}", distance, env.values);

        env.values.get(name).unwrap().clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_into_string() {
        let foo: Result<String, _> = Value::String("foo".to_string()).try_into();
        assert_eq!(foo.unwrap(), "foo");
    }
}
