with Tokens; use Tokens;
procedure Adalox is
   T : Token;
begin
   T := Create_Token (TK => TOK_LEFT_PAREN, S => "Lolex");
   Print_Lexeme (T);
end Adalox;
