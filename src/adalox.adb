with Tokens;         use Tokens;
with Scanners;       use Scanners;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

procedure Adalox is
   T      : Token;
   Tokens : Token_Vector;
begin
   T := Create_Token (TK => TOK_LEFT_PAREN, S => "Lolex");
   Print_Lexeme (T);
   Do_Nothing;
   Tokens.Append (T);
   Tokens.Append (T);
   Tokens.Append (T);
   Tokens.Append (T);
   Tokens.Append (T);

   Put_Line ("Vector has" & Count_Type'Image (Tokens.Length) & " elements");
end Adalox;
