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

   procedure Print_Lexeme (T : Token) is
   begin
      Put_Line (To_String (T.Lexeme));
   end Print_Lexeme;

   procedure Print_Token (T : Token) is
   begin
      Put_Line
        ("Lexeme: " & To_String (T.Lexeme) & " | Kind: " & T.Kind'Image);
   end Print_Token;

   function Lexeme_String_Hash
     (LS : Lexeme_String) return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash (To_String (LS));
   end Lexeme_String_Hash;
end Tokens;
