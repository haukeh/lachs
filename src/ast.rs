use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::scanner::{LiteralValue, Token};

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

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(Token, Option<Expr>),
    Block(Vec<Stmt>)
}

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, LiteralValue>,
    enclosing: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None
        }
    }

    pub fn with_enclosing(enc: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enc),         
        }
    }

    pub fn define(&mut self, name: String, val: LiteralValue) {
        self.values.insert(name, val);
    }

    pub fn assign(&mut self, name: &Token, val: LiteralValue) {
        if let Some(local) = self.values.get_mut(&name.lexeme) {
            *local = val;
        } else if let Some(parent_env) = self.enclosing.as_ref() {                
            parent_env.borrow_mut().assign(name, val);
        }           
        
        panic!("Assign to undefined variable {}", name.lexeme)        
    }

    pub fn get(&self, name: &str) -> LiteralValue {
        if let Some(local) = self.values.get(name) {
            return local.clone()
        }

        if let Some(parent_env) = self.enclosing.as_ref() {
            let p = parent_env.borrow();
            return p.get(name)
        }
        
        panic!("Undefined variable {}", name);       
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

pub trait ExpressionVisitor<T> {
    fn visit_expr(&mut self, e: &Expr, env: Rc<RefCell<Environment>>) -> T;
}

pub trait StatementVisitor<T> {
    fn visit_stmt(&mut self, e: &Stmt, env: Rc<RefCell<Environment>>) -> T;
}
