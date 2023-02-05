use crate::{
    ast::{Expr, Stmt},
    interpreter::Interpreter,
    parser::FunctionKind,
    scanner::Token,
};

use anyhow::anyhow;
use log::debug;
use std::collections::HashMap;

pub struct Resolver<'a, 'b> {
    interpreter: &'a mut Interpreter<'b>,
    scopes: Vec<HashMap<String, bool>>,
    current_fn: Option<FunctionKind>,
}

impl<'a, 'b> Resolver<'a, 'b> {
    pub fn new(i: &'a mut Interpreter<'b>) -> Self {
        Resolver {
            interpreter: i,
            scopes: Vec::new(),
            current_fn: None,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    pub fn resolve_stmt(&mut self, stmt: &Stmt) -> anyhow::Result<()> {
        match stmt {
            Stmt::Expression(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::If { cond, then, elze } => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(then)?;
                if let Some(s) = elze.as_ref() {
                    self.resolve_stmt(s)?
                }
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Return(_, value) => {
                if self.current_fn.is_none() {
                    return Err(anyhow!("Can't return from top level code."));
                }
                self.resolve_expr(value)?;
            }
            Stmt::Var(name, init) => {
                self.declare(name);
                if let Some(expr) = init {
                    self.resolve_expr(expr)?;
                }
                self.define(name);
            }
            Stmt::While(cond, body) => {
                self.resolve_expr(cond)?;
                self.resolve_stmt(body)?;
            }
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve_stmts(stmts)?;
                self.end_scope();
            }
            Stmt::Function(f) => {
                let enclosing_fn = self.current_fn.take();
                self.current_fn = Some(FunctionKind::Func);
                self.declare(&f.name);
                self.define(&f.name);
                self.begin_scope();
                for param in &f.params {
                    self.declare(param);
                    self.define(param);
                }
                self.resolve_stmts(&f.body)?;
                self.end_scope();
                self.current_fn = enclosing_fn;
            }
        };
        Ok(())
    }

    pub fn resolve_stmts(&mut self, stmts: &Vec<Stmt>) -> anyhow::Result<()> {
        debug!("Before: {:?}", self.scopes);
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        debug!("After: {:?}", self.scopes);
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Unary {
                id: _,
                op: _,
                right,
            } => {
                self.resolve_expr(right)?;
            }
            Expr::Logical {
                id: _,
                left,
                op: _,
                right,
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Binary {
                id: _,
                left,
                op: _,
                right,
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call {
                id: _,
                callee,
                paren: _,
                args,
            } => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Assign { id: _, name, value } => {
                self.resolve_expr(value)?;
                self.resolve_local(expr, name);
            }
            Expr::Grouping { id: _, expr } => {
                self.resolve_expr(expr)?;
            }
            Expr::Literal(_, _) => {}
            Expr::Variable(_, name) => {
                if let Some(false) = self.scopes.last().and_then(|scope| scope.get(&name.lexeme)) {
                    return Err(anyhow!("Can't read local variable in its own initializer."));
                }
                self.resolve_local(expr, name);
            }
        }
        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.get(&name.lexeme).is_some() {
                self.interpreter.resolve(expr, self.scopes.len() - i - 1);
            }
        }
    }
}
