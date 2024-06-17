with AST;      use AST;
with Literals; use Literals;
with Tokens;
package Interpreters is
   Runtime_Error : exception;
   procedure Interpret (Statements : AST.Stmt_Vector);
private
   function Is_Truthy (L : Literal) return Boolean;
   function Is_Equal (A : Literal; B : Literal) return Boolean;
   procedure Check_Number_Operand (T : Tokens.Token; Operand : Literal);
   procedure Check_Number_Operands
     (T : Tokens.Token; Left : Literal; Right : Literal);
   function Stringify (L : Literal) return String;
   function Evaluate_Expr (E : Expr_Access) return Literal;
   function Evaluate_Binary_Expr (E : Expr_Access) return Literal;
   function Evaluate_Unary_Expr (E : Expr_Access) return Literal;
   function Evaluate_Grouping_Expr (E : Expr_Access) return Literal;
end Interpreters;
