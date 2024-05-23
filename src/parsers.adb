with AST;      use AST;
with Literals; use Literals;
package body Parsers is

   function Parse (Tokens : Token_Vector) return Expr is
      Current : Natural;

      function Peek return Token is
      begin
         return Tokens (Current);
      end Peek;

      function Is_At_End return Boolean is
      begin
         return Peek.Kind = End_Of_File_Token;
      end Is_At_End;
   begin
      --  while not Is_At_End loop
      --  end loop;

      return Expr'(Literal_Kind_Type, (Kind => Nothing));

   end Parse;

end Parsers;
