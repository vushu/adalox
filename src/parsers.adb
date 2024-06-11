with AST;         use AST;
with Literals;    use Literals;
with Tokens;      use Tokens;
with Ada.Text_IO; use Ada.Text_IO;
package body Parsers is

   function Parse (Tokens : Token_Vector) return Expr is
      Current : Natural := 0;

      function Peek return Token is
      begin
         return Tokens (Current);
      end Peek;

      function Previous return Token is
      begin
         return Tokens (Current - 1);
      end Previous;

      function Is_At_End return Boolean is
      begin
         return Peek.Kind = End_Of_File_Token;
      end Is_At_End;

      function Advance return Token is
      begin
         if Is_At_End then
            Current := Current + 1;
         end if;
         return Previous;
      end Advance;

      procedure Skip is
      begin
         if Is_At_End then
            Current := Current + 1;
         end if;
      end Skip;

      function Match return Boolean is
         K   : constant Token_Kind := True_Token;
         Tok : Token               :=
           Create_Token
             (TK => False_Token, S => "False",
              L  => (Kind => Bool_Type, Bool_Val => False));
         P   : Token               := Peek;

      begin
         P.Line := 103;
         Print_Token (P);
         return True;
      end Match;

      --  function Primary return Expr_Access is
      --  begin
      --  end Primary;

      --  function Expression return Expr is
      --  begin

      --  end Expression;
   begin
      --  while not Is_At_End loop
      --  end loop;

      return Expr'(Literal_Kind_Type, (Kind => Nothing));

   end Parse;

end Parsers;
