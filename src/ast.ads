with Tokens;   use Tokens;
with Literals; use Literals;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
package AST is

   type Expr;
   type Expr_Access is access Expr;
   type Expr_Kind is
     (Unary_Kind_Type, Binary_Kind_Type, Grouping_Kind_Type, Literal_Kind_Type,
      Variable_Kind_Type, Logical_Kind_Type);

   type Expr (Kind : Expr_Kind) is record
      case Kind is
         when Unary_Kind_Type =>
            Unary_Right : Expr_Access;
            Unary_Op    : Token;
         when Binary_Kind_Type | Logical_Kind_Type =>
            Left  : Expr_Access;
            Right : Expr_Access;
            Op    : Token;
         when Grouping_Kind_Type =>
            Expression : Expr_Access;
         when Literal_Kind_Type =>
            Value : Literal;
         when Variable_Kind_Type =>
            Tok : Token;
      end case;
   end record;

   package Expr_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Expr_Access);
   subtype Expr_Vector is Expr_List.Vector;

end AST;
