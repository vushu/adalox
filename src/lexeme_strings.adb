with Ada.Text_IO; use Ada.Text_IO;
package body Lexeme_Strings is

   function "=" (Left : Lexeme_String; Right : String) return Boolean is
   begin
      return To_String (Source => Left) = Right;
   end "=";

   function Make_Lexeme_String (Source : String) return Lexeme_String is
      Lex_String : Lexeme_String;
      Len        : constant Natural := Source'Length;
   begin
      --  This will init the array to be all in whitespaces.
      Lex_String.Data   := (others => ' ');
      Lex_String.Length := 0;

      if Len <= Max then
         Lex_String.Data (1 .. Len) := Source;
         Lex_String.Length          := Length_T (Len);
      else
         raise Program_Error;
      end if;

      return Lex_String;
   end Make_Lexeme_String;

   function To_String (Source : Lexeme_String) return String is
   begin
      return Source.Data (1 .. Natural (Source.Length));
   end To_String;
end Lexeme_Strings;
