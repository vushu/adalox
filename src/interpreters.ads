with AST;      use AST;
with Literals; use Literals;
with Tokens;
package Interpreters is
   Runtime_Error : exception;
   procedure Interpret (Statements : AST.Stmt_Vector);
   --  function Interpret (Statements : AST.Stmt_Vector) return Lit;
private
   procedure Check_Number_Operand (T : Tokens.Token; Operand : Literal);
   procedure Check_Number_Operands
     (T : Tokens.Token; Left : Literal; Right : Literal);
   function Evaluate_Expr (E : Expr_Access) return Literal;
   function Evaluate_Binary_Expr (E : Expr_Access) return Literal;
   function Evaluate_Logical_Expr (E : Expr_Access) return Literal;
   function Evaluate_Unary_Expr (E : Expr_Access) return Literal;
   function Evaluate_Grouping_Expr (E : Expr_Access) return Literal;
   procedure Evaluate_Print_Stmt (S : Stmt_Access);
   procedure Evaluate_Expression_Stmt (S : Stmt_Access);
   procedure Execute (Stmt : Stmt_Access);
end Interpreters;
