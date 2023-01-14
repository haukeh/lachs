use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::scanner::{LiteralValue, Token, TokenType};

pub trait TokenHolder<'a> {
    fn token(&'a self) -> Option<&'a Token>;
}

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

impl Expr {
    pub fn token(&self) -> Option<&Token> {
        match self {
            Expr::Unary { op, .. } => Some(op),
            Expr::Binary { op, .. } => Some(op),
            Expr::Assign { name, .. } => Some(name),
            Expr::Grouping { expr } => expr.token(),
            Expr::Literal(_) => None,
            Expr::Variable(t) => Some(t),
        }
    }
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
    fn visit_expr<'a>(&mut self, e: &'a Expr, env: Rc<RefCell<Environment>>) -> T;
}

pub trait StatementVisitor<T> {
    fn visit_stmt(&mut self, e: &Stmt, env: Rc<RefCell<Environment>>) -> T;
}


#[cfg(test)]
mod tests {
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
