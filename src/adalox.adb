with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Tokens;      use Tokens;
procedure Adalox is
   --package S renames Ada.Strings;
   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);
   T : Token                      := Create_Token;
   U : constant String            := "MAMAMsdfasdfasdfasdf";
   X : constant SB.Bounded_String := SB.To_Bounded_String (U);
begin
   --Put_Line (SB.To_String (X));
   Print_Lexeme (T);
end Adalox;
