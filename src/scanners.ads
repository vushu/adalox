with Tokens; use Tokens;
package Scanners is
   function Scan_Tokens (Src : String) return Token_Vector;
   procedure Scan_Token;

private
   procedure Add_Token (TK : Token_Kind);
   procedure Add_Token (TK : Token_Kind; Lexeme : String);

   ------- STATE
   Start       : Natural              := 1;
   Source      : String (1 .. 10_240) := (others => ' ');
   Tokens      : Token_Vector;
   Current     : Natural;
   Line        : Natural;
   Source_Size : Positive;
end Scanners;
