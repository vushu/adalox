with Ada.Text_IO; use Ada.Text_IO;
package body Tokens is
   function Create_Token (TK : Token_Kind) return Token is
      T : Token;
   begin
      T.Kind   := TK;
      T.Line   := 1;
      T.Lexeme := Make_Lexeme_String ("");
      return T;
   end Create_Token;

   function Create_Token (TK : Token_Kind; S : String) return Token is
      T : Token;
   begin
      T.Lexeme := Make_Lexeme_String (S);
      T.Kind   := TK;
      T.Line   := 1;
      return T;
   end Create_Token;

   procedure Print_Lexeme (T : Token) is
   begin
      Put_Line (To_String (T.Lexeme));
   end Print_Lexeme;

end Tokens;
