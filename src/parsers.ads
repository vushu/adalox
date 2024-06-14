with Tokens; use Tokens;
with AST;
with Ada.Containers.Vectors;
package Parsers is
   function Parse (Tokens : Token_Vector) return AST.Expr_Vector;
end Parsers;
