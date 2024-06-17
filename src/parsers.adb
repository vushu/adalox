with AST;            use AST;
with Literals;       use Literals;
with Ada.Text_IO;    use Ada.Text_IO;
with Lexeme_Strings; use Lexeme_Strings;
with Error_Reports;
package body Parsers is

   function Parse (Tokens : Token_Vector) return Stmt_Vector is
      Parser_Error  : exception;
      Runtime_Error : exception;
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
         if not Is_At_End then
            Current := Current + 1;
         end if;
         return Previous;
      end Advance;

      procedure Skip is
      begin
         if not Is_At_End then
            Current := Current + 1;
         end if;
      end Skip;

      function Check (T : Token_Kind) return Boolean is
      begin
         if Is_At_End then
            return False;
         end if;
         return Peek.Kind = T;
      end Check;

      type Variadic_Token_Kinds is array (Positive range <>) of Token_Kind;

      function Match (Types : Variadic_Token_Kinds) return Boolean is
      begin
         for T of Types loop
            if Check (T) then
               Skip;
               return True;
            end if;
         end loop;
         return False;
      end Match;

      function Consume (T : Token_Kind; M : String) return Token is
      begin
         if Check (T) then
            return Advance;
         end if;

         Put_Line ("Error expected: " & M);
         raise Parser_Error;
      end Consume;

      procedure Consume_Skip (T : Token_Kind; M : String) is
      begin
         if Check (T) then
            Skip;
            return;
         end if;
         Put_Line ("Error expected: " & M);
         raise Parser_Error;
      end Consume_Skip;

      procedure Synchronize is
      begin
         Skip;
         if Previous.Kind = Semicolon_Token then
            return;
         end if;
         case Peek.Kind is
            when Class_Token | Fun_Token | Var_Token | For_Token | If_Token
              | While_Token | Print_Token | Return_Token =>
               return;
            when others =>
               Skip;
         end case;
      end Synchronize;

      function Statement return Stmt_Access is

         function Expression return Expr_Access is

            function Primary return Expr_Access is
            begin
               if Match ((1 => False_Token)) then
                  return new Expr'(Literal_Kind_Type, (Bool_Type, False));
               elsif Match ((1 => True_Token)) then
                  return new Expr'(Literal_Kind_Type, (Bool_Type, True));
               elsif Match ((1 => Nil_Token)) then
                  return new Expr'(Literal_Kind_Type, (Kind => Nothing));
               elsif Match ((Number_Token, String_Token)) then
                  return
                    new Expr'(Literal_Kind_Type, (Previous.Literal_Value));
               elsif Match ((1 => Identifier_Token)) then
                  return new Expr'(Variable_Kind_Type, (Previous));
               elsif Match ((1 => Left_Paren_Token)) then
                  Consume_Skip
                    (Right_Paren_Token, "expected ) after expression.");
                  return new Expr'(Grouping_Kind_Type, (Expression));
               end if;

               Error_Reports.Error (Peek, "Expect expression.");
               raise Parser_Error;

            end Primary;

            function Unary return Expr_Access is
            begin
               if Match ((Bang_Token, Minus_Token)) then
                  return new Expr'(Unary_Kind_Type, Unary, Previous);
               end if;
               return Primary;
            end Unary;

            function Factor return Expr_Access is
               E : Expr_Access := Unary;
            begin
               while Match ((Slash_Token, Star_Token)) loop
                  E :=
                    new Expr'
                      (Kind => Binary_Kind_Type, Left => E, Right => Unary,
                       Op   => Previous);
               end loop;
               return E;
            end Factor;

            function Term return Expr_Access is
               E : Expr_Access := Factor;
            begin
               while Match ((Minus_Token, Plus_Token)) loop
                  E :=
                    new Expr'
                      (Kind => Binary_Kind_Type, Left => E, Right => Factor,
                       Op   => Previous);
               end loop;
               return E;
            end Term;

            function Comparison return Expr_Access is
               E : Expr_Access := Term;
            begin
               while Match
                   ((Greater_Token, Greater_Equal_Token, Less_Token,
                     Less_Equal_Token))
               loop
                  E :=
                    new Expr'
                      (Kind => Binary_Kind_Type, Left => E, Right => Factor,
                       Op   => Previous);
               end loop;
               return E;
            end Comparison;

            function Equality return Expr_Access is
               E : Expr_Access := Comparison;
            begin
               while Match ((Bang_Equal_Token, Equal_Equal_Token)) loop
                  E :=
                    new Expr'
                      (Kind  => Binary_Kind_Type, Left => E,
                       Right => Comparison, Op => Previous);
               end loop;
               return E;
            end Equality;

            function And_Expression return Expr_Access is
               E : Expr_Access := Equality;
            begin
               while Match ((1 => And_Token)) loop
                  E :=
                    new Expr'
                      (Kind => Logical_Kind_Type, Left => E, Right => Equality,
                       Op   => Previous);
               end loop;
               return E;
            end And_Expression;

            function Or_Expression return Expr_Access is
               E : Expr_Access := And_Expression;
            begin
               while Match ((1 => Or_Token)) loop
                  E :=
                    new Expr'
                      (Kind  => Logical_Kind_Type, Left => E,
                       Right => And_Expression, Op => Previous);
               end loop;
               return E;
            end Or_Expression;

         begin
            return Or_Expression;
         end Expression;

         function ExpressionStatement return Stmt_Access is
            E : Expr_Access := Expression;
         begin
            Consume_Skip (Semicolon_Token, "Expected ';' after expression.");
            return new Stmt'(Expression_Kind_Type, E);
         end ExpressionStatement;

      begin
         return ExpressionStatement;

      end Statement;

      Stmts : Stmt_Vector;
   begin

      while not Is_At_End loop
         Stmts.Append (Statement);
      end loop;
      return Stmts;
   end Parse;

end Parsers;
