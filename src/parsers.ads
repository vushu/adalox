with Tokens; use Tokens;
with AST;
package Parsers is
   function Parse (Tokens : Token_Vector) return AST.Expr;
end Parsers;
