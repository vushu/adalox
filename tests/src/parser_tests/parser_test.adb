with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;      use Ada.Text_IO;
with AST;              use AST;
with Tokens;           use Tokens;
with Literals;         use Literals;
with Parsers;          use Parsers;
with Lexeme_Strings;   use Lexeme_Strings;
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

   procedure Testing_Slice is
      S    : constant String  := "1234.0";
      L    : constant String  := S (S'Last - 1 .. S'Last);
      HEAD : constant String  := S (1 .. S'Last - 2);
      B    : constant Boolean := (L = ".0");
   begin
      Put_Line ("Slice: " & L);
      Put_Line ("Slice: " & B'Image);
      Put_Line ("HEAD: " & HEAD);
      Assert (HEAD = "1234", "Should be equal");

   end Testing_Slice;

   procedure Should_Parse is
      Tokens        : Token_Vector;
      Tok_Minus     : constant Token :=
        Create_Token (Minus_Token, "-", Literal'(Kind => Nothing));
      Tok_Number    : constant Token :=
        Create_Token (Number_Token, "1", Literal'(Float_Type, 1.0));
      Tok_Semicolon : constant Token :=
        Create_Token
          (Semicolon_Token, ";", (String_Type, Make_Lexeme_String (";")));

      Res : Stmt_Vector;
   begin
      Tokens.Append (Tok_Minus);
      Tokens.Append (Tok_Number);
      Tokens.Append (Tok_Semicolon);
      Tokens.Append (Create_EOF_Token);
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
      Testing_Slice;
      Should_Parse;
   end Run_Test;

end Parser_Test;
