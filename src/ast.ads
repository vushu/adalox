with Tokens;   use Tokens;
with Literals; use Literals;
package AST is

   type Expr;
   type Expr_Access is access Expr;
   type Expr_Kind is
     (Unary_Kind_Type, Binary_Kind_Type, Grouping_Kind_Type, Literal_Kind_Type,
      Variable_Kind_Type, Logical_Kind_Type);
   type Expr (Kind : Expr_Kind) is record
      case Kind is
         when Unary_Kind_Type | Binary_Kind_Type | Logical_Kind_Type =>
            Left  : Expr_Access;
            Right : Expr_Access;
            Op    : Token;
         when Grouping_Kind_Type =>
            Expression : Expr_Access;
         when Literal_Kind_Type =>
            Literal_Kind_Value : Literal;
         when Variable_Kind_Type =>
            Tok : Token;
      end case;
   end record;

end AST;
