with Ada.Text_IO; use Ada.Text_IO;
package body Tokens is
   procedure Do_Stuff is
   begin
      Put_Line ("safsafdasdfa");
   end Do_Stuff;

   function Create_Token return Token is
      T : Token;
   begin
      T.Kind   := LEFT_PAREN;
      T.Lexeme := SB.To_Bounded_String ("MAMA");
      T.Line   := 100;
      return T;
   end Create_Token;

   procedure Print_Lexeme (T : Token) is
   begin
      Put_Line (SB.To_String (T.Lexeme));
   end Print_Lexeme;

end Tokens;
