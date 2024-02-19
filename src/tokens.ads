with Ada.Strings.Bounded;
package Tokens is
   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);

   type Token_Kind is (LEFT_PAREN, RIGHT_PAREN);
   type Token is record
      Kind   : Token_Kind;
      Lexeme : SB.Bounded_String;
      Line   : Positive;
   end record;
   procedure Do_Stuff;
   procedure Print_Lexeme (T : Token);
   function Create_Token return Token;
end Tokens;
