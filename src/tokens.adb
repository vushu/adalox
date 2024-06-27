with Ada.Text_IO;    use Ada.Text_IO;
with Lexeme_Strings; use Lexeme_Strings;
package body Tokens is
   function Create_Token
     (TK : Token_Kind; S : String; L : Literal) return Token
   is
      T : Token;
   begin
      T.Lexeme        := Make_Lexeme_String (S);
      T.Literal_Value := L;
      T.Kind          := TK;
      T.Line          := 1;
      return T;
   end Create_Token;

   function Create_EOF_Token return Token is
   begin
      return
        Create_Token
          (TK => End_Of_File_Token, S => "", L => (Kind => Nothing));
   end Create_EOF_Token;

   function Set_Line (T : in out Token; Line : Positive) return Token is
   begin
      T.Line := Line;
      return T;
   end Set_Line;

   procedure Print_Lexeme (T : Token) is
   begin
      Put_Line (To_String (T.Lexeme));
   end Print_Lexeme;

   procedure Print_Token (T : Token) is
   begin
      Put_Line
        ("Lexeme: " & To_String (T.Lexeme) & " | Kind: " & T.Kind'Image &
         " Line: " & T.Line'Image);
   end Print_Token;
end Tokens;
