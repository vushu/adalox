with AST;               use AST;
with Literals;          use Literals;
with Tokens;
with Environments;      use Environments;
with Lox_Primitives; use Lox_Primitives;
package Interpreters is
   type Interpreter is tagged record
      Env : Environment_Access;
   end record;

   Runtime_Error : exception;
   procedure Interpret (Statements : AST.Stmt_Vector);
   --  function Interpret (Statements : AST.Stmt_Vector) return Lit;
private
   Env : Environment_Access;
   procedure Check_Number_Operand (T : Tokens.Token; Operand : Literal);
   procedure Check_Number_Operands
     (T : Tokens.Token; Left : Literal; Right : Literal);
   function Evaluate_Expr (E : Expr_Access) return Lox_Primitive;
   function Evaluate_Binary_Expr (E : Expr_Access) return Lox_Primitive;
   function Evaluate_Logical_Expr (E : Expr_Access) return Lox_Primitive;
   function Evaluate_Unary_Expr (E : Expr_Access) return Lox_Primitive;
   function Evaluate_Grouping_Expr (E : Expr_Access) return Lox_Primitive;
   function Evaluate_Variable_Expr (E : Expr_Access) return Lox_Primitive;
   function Evaluate_Assign_Expr (E : Expr_Access) return Lox_Primitive;
   --  function Evaluate_Call_Expr (E : Expr_Access) return Literal;
   procedure Evaluate_Print_Stmt (S : Stmt_Access);
   procedure Evaluate_If_Stmt (S : Stmt_Access);
   procedure Evaluate_Var_Decl_Stmt (S : Stmt_Access);
   procedure Evaluate_While_Stmt (S : Stmt_Access);
   procedure Evaluate_Expression_Stmt (S : Stmt_Access);
   procedure Evaluate_Block_Stmt (S : Stmt_Access);
   procedure Execute_Block (Statements : Stmt_Vector; E : Environment_Access);

   procedure Execute (Stmt : Stmt_Access);
end Interpreters;
