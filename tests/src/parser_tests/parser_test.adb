with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;      use Ada.Text_IO;
with AST;              use AST;
with Tokens;           use Tokens;
with Literals;         use Literals;
with Parsers;          use Parsers;
package body Parser_Test is

   procedure Can_Create_Expr is
      Tok          : constant Token       :=
        Create_Token (Minus_Token, "-", Literal'(Kind => Nothing));
      Literal_Expr : constant Expr_Access :=
        new Expr'(Literal_Kind_Type, (Int_Type, 1));
      Unary_Expr : constant Expr := Expr'(Unary_Kind_Type, Literal_Expr, Tok);
   begin
      Assert (Unary_Expr.Unary_Right.Value.Int_Val = 1, "Should be equal.");
   end Can_Create_Expr;

   procedure Should_Parse is
      Tokens : Token_Vector;
      Tok    : constant Token :=
        Create_Token (Minus_Token, "-", Literal'(Kind => Nothing));

      Res : Expr := Expr'(Literal_Kind_Type, (Kind => Nothing));
   begin
      Tokens.Append (Tok);
      Res := Parse (Tokens);
   end Should_Parse;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Parsers package");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Can_Create_Expr;
      Should_Parse;
   end Run_Test;

end Parser_Test;
