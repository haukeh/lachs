
pub struct AstPrinter {}

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }

    pub fn print(&mut self, expr: &Expr) {
        println!("{}", self.visit_expr(expr));
    }

    pub fn string(&mut self, expr: &Expr) -> String {
        self.visit_expr(expr)
    }
}

impl ExpressionVisitor<String> for AstPrinter {
    fn visit_expr<'a>(&self, e: &'a Expr) -> String {
        match e {
            Expr::Unary { op, right } => {
                let res = self.visit_expr(right);
                format!("({} {})", op.lexeme, res)
            }
            Expr::Binary {
                ref left,
                ref op,
                ref right,
            } => {
                let lhs = self.visit_expr(left);
                let rhs = self.visit_expr(right);
                format!("({} {} {})", op.lexeme, lhs, rhs)
            }
            Expr::Grouping { expr } => {
                let res = self.visit_expr(expr);
                format!("(group {})", res)
            }
            Expr::Literal(value) => format!("{}", value),
            Expr::Variable(var) => {
                let value = var.literal.as_ref().unwrap_or(&LiteralValue::Nil);
                format!("var {} = {}", var.lexeme, value)
            }
            Expr::Assign { name, value } => {
                
            },
        }
    }
}