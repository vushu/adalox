with Lexeme_Strings; use Lexeme_Strings;
package Tokens is

   type Token_Kind is (LEFT_PAREN, RIGHT_PAREN);
   type Token is record
      Kind   : Token_Kind;
      Lexeme : Lexeme_String;
      Line   : Positive;
   end record;
   procedure Do_Stuff;
   procedure Print_Lexeme (T : Token);
   function Create_Token return Token;
   function Create_Token (TK : Token_Kind; S : String) return Token;
end Tokens;
