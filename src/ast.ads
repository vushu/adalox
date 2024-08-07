with Tokens;         use Tokens;
with Literals;       use Literals;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
--  with Lox_Primitives; use Lox_Primitives;
package AST is
   type Expr;
   type Expr_Access is access Expr;

   package Expr_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Expr_Access);
   subtype Expr_Vector is Expr_List.Vector;

   type Expr_Kind is
     (Unary_Kind_Type, Binary_Kind_Type, Grouping_Kind_Type, Literal_Kind_Type,
      Variable_Kind_Type, Logical_Kind_Type, Assign_Kind_Type, Call_Kind_Type);

   type Expr (Kind : Expr_Kind) is record
      case Kind is
         when Unary_Kind_Type =>
            Unary_Right : Expr_Access;
            Unary_Op    : Token;
         when Assign_Kind_Type =>
            Assign_Name  : Token;
            Assign_Value : Expr_Access;
         when Binary_Kind_Type | Logical_Kind_Type =>
            Left  : Expr_Access;
            Right : Expr_Access;
            Op    : Token;
         when Grouping_Kind_Type =>
            Expression : Expr_Access;
         when Literal_Kind_Type =>
            Value : Literal;
         when Variable_Kind_Type =>
            Variable_Name : Token;
         when Call_Kind_Type =>
            Callee    : Expr_Access;
            Paren     : Token;
            Arguments : Expr_Vector;
      end case;
   end record;

   type Stmt;
   type Stmt_Access is access Stmt;
   type Stmt_Kind is
     (
   --  Block_Kind_Type,
   --  Class_Kind_Type,
   Expression_Kind_Type,
                     --  Function_Stmt_Kind_Type,
                     If_Stmt_Kind_Type, Print_Stmt_Kind_Type,
                     --  Return_Stmt_Kind_Type,
                     Var_Decl_Stmt_Kind_Type, Block_Kind_Type,
      While_Stmt_Kind_Type);

   package Stmt_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Stmt_Access);
   subtype Stmt_Vector is Stmt_List.Vector;

   type Stmt (Kind : Stmt_Kind) is record
      case Kind is
         when Expression_Kind_Type | Print_Stmt_Kind_Type =>
            Expression : Expr_Access;
         when Var_Decl_Stmt_Kind_Type =>
            Name        : Token;
            Initializer : Expr_Access;
         when Block_Kind_Type =>
            Block_Statements : Stmt_Vector;
         when If_Stmt_Kind_Type =>
            If_Condition   : Expr_Access;
            If_Then_Branch : Stmt_Access;
            If_Else_Branch : Stmt_Access;
         when While_Stmt_Kind_Type =>
            While_Condition : Expr_Access;
            While_Body      : Stmt_Access;
      end case;
   end record;
end AST;
