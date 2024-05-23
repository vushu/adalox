with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;      use Ada.Text_IO;
with AST;              use AST;
with Tokens;           use Tokens;
with Lexeme_Strings;   use Lexeme_Strings;
with Literals;         use Literals;
package body Parser_Test is
   type Expr_Access is access Expr;

   procedure Can_Create_Expr is
      LIT : Literal (Kind => String_Type);
      T   : Token;
      E   : Expr_Access := new Expr (Kind => Literal_Kind_Type);
   begin
      LIT.String_Val := Make_Lexeme_String ("Heje");
      T              := Create_Token (TK => TOK_STRING, S => "Heje", L => LIT);

   end Can_Create_Expr;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Scanners package");
   end Name;
   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Can_Create_Expr;
   end Run_Test;

end Parser_Test;
