with Ada.Text_IO; use Ada.Text_IO;
with Tokens;      use Tokens;
procedure Adalox is
   T : Token;
begin
   T := Create_Token (TK => LEFT_PAREN, S => "Lolex");
   Print_Lexeme (T);
end Adalox;
