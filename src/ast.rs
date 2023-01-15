use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::debug;

use crate::scanner::{LiteralValue, Token};

#[derive(Debug)]
pub enum Expr {
    Unary {
        op: Token,
        right: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Literal(LiteralValue),
    Variable(Token),
}

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    If {
        cond: Expr,
        then: Box<Stmt>,
        elze: Option<Box<Stmt>>,
    },
    Print(Expr),
    Var(Token, Option<Expr>),
    While(Expr, Box<Stmt>),
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, LiteralValue>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with_enclosing(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(parent),
        }
    }

    pub fn define(&mut self, name: String, val: LiteralValue) {
        self.values.insert(name, val);
    }

    pub fn assign(&mut self, name: &Token, val: LiteralValue) {
        debug!("Assignment {:?}={} [env: {:?}", name.lexeme, val, self);
        
        if let Some(local) = self.values.get_mut(&name.lexeme) {
            *local = val;
        } else if let Some(parent_env) = self.enclosing.as_ref() {
            parent_env.borrow_mut().assign(name, val);
        } else {
            panic!("Assign to undefined variable {}", name.lexeme)
        }
    }

    pub fn get(&self, name: &str) -> LiteralValue {
        debug!("Looking up {} in {:?}", name, self);

        if let Some(local) = self.values.get(name) {
            debug!("Found: {}", local);
            return local.clone()
        }

        if let Some(parent_env) = self.enclosing.as_ref() {
            let p = parent_env.borrow();
            return p.get(name)
        }
    
        panic!("Undefined variable {}", name);
    }
}

pub trait ExpressionVisitor<T> {
    fn visit_expr(&mut self, e: &Expr, env: Rc<RefCell<Environment>>) -> T;
}

pub trait StatementVisitor<T> {
    fn visit_stmt(&mut self, e: &Stmt, env: Rc<RefCell<Environment>>) -> T;
}
