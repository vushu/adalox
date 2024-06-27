with AST;            use AST;
with Literals;       use Literals;
with Ada.Text_IO;    use Ada.Text_IO;
with Lexeme_Strings; use Lexeme_Strings;
with Error_Reports;
package body Parsers is

   function Peek (Self : Parser) return Token is
   begin
      return Self.Tokens (Self.Current);
   end Peek;
   function Is_At_End (Self : Parser) return Boolean is
   begin
      return Self.Peek.Kind = End_Of_File_Token;
   end Is_At_End;

   function Previous (Self : Parser) return Token is
   begin
      return Self.Tokens (Self.Current - 1);
   end Previous;

   function Advance (Self : in out Parser) return Token is
   begin
      if not Self.Is_At_End then
         Self.Current := Self.Current + 1;
      end if;
      return Self.Previous;
   end Advance;

   function Match
     (Self : in out Parser; Types : Variadic_Token_Kinds) return Boolean
   is
   begin
      for T of Types loop
         if Self.Check (T) then
            Skip (Self);
            return True;
         end if;
      end loop;
      return False;
   end Match;

   procedure Skip (Self : in out Parser) is
   begin
      if not Self.Is_At_End then
         Self.Current := Self.Current + 1;
      end if;
   end Skip;

   function Check (Self : Parser; T : Token_Kind) return Boolean is
   begin
      if Self.Is_At_End then
         return False;
      end if;
      return Self.Peek.Kind = T;
   end Check;

   procedure Consume_Skip (Self : in out Parser; T : Token_Kind; M : String) is
   begin
      if Self.Check (T) then
         Skip (Self);
         return;
      end if;
      raise Parser_Error with "Error expected: " & M;
   end Consume_Skip;

   function Consume
     (Self : in out Parser; T : Token_Kind; M : String) return Token
   is
   begin
      if Self.Check (T) then
         return Self.Advance;
      end if;

      raise Parser_Error with "Error expected: " & M;
   end Consume;

   function Expression (Self : in out Parser) return Expr_Access is

      function Primary return Expr_Access is
      begin
         if Match (Self, (1 => False_Token)) then
            return new Expr'(Literal_Kind_Type, (Bool_Type, False));
         elsif Match (Self, (1 => True_Token)) then
            return new Expr'(Literal_Kind_Type, (Bool_Type, True));
         elsif Match (Self, (1 => Nil_Token)) then
            return new Expr'(Literal_Kind_Type, (Kind => Nothing));
         elsif Match (Self, (Number_Token, String_Token)) then
            return new Expr'(Literal_Kind_Type, (Self.Previous.Literal_Value));
         elsif Match (Self, (1 => Identifier_Token)) then
            return new Expr'(Variable_Kind_Type, (Self.Previous));
         elsif Match (Self, (1 => Left_Paren_Token)) then
            declare
               E : constant Expr_Access := Self.Expression;
            begin
               Self.Consume_Skip
                 (Right_Paren_Token, "expected ) after expression.");
               return new Expr'(Grouping_Kind_Type, E);
            end;
         end if;

         Error_Reports.Error (Self.Peek, "Expect expression.");
         raise Parser_Error;
      end Primary;

      function Unary return Expr_Access is
      begin
         if Self.Match ((Bang_Token, Minus_Token)) then
            declare
               Op    : constant Token       := Self.Previous;
               Right : constant Expr_Access := Unary;
            begin
               return new Expr'(Unary_Kind_Type, Right, Op);
            end;
         end if;
         return Primary;
      end Unary;

      function Factor return Expr_Access is
         E : Expr_Access := Unary;
      begin
         while Self.Match ((Slash_Token, Star_Token)) loop
            declare
               Op    : constant Token       := Self.Previous;
               Right : constant Expr_Access := Unary;
            begin

               E :=
                 new Expr'
                   (Kind => Binary_Kind_Type, Left => E, Right => Right,
                    Op   => Op);
            end;
         end loop;
         return E;
      end Factor;

      function Term return Expr_Access is
         E : Expr_Access := Factor;
      begin
         while Self.Match ((Minus_Token, Plus_Token)) loop
            declare
               Op    : constant Token       := Self.Previous;
               Right : constant Expr_Access := Factor;
            begin

               E :=
                 new Expr'
                   (Kind => Binary_Kind_Type, Left => E, Right => Right,
                    Op   => Op);
            end;
         end loop;
         return E;
      end Term;

      function Comparison return Expr_Access is
         E : Expr_Access := Term;
      begin
         while Self.Match
             ((Greater_Token, Greater_Equal_Token, Less_Token,
               Less_Equal_Token))
         loop
            declare
               Op    : constant Token       := Self.Previous;
               Right : constant Expr_Access := Factor;
            begin
               E :=
                 new Expr'
                   (Kind => Binary_Kind_Type, Left => E, Right => Right,
                    Op   => Op);
            end;
         end loop;
         return E;
      end Comparison;

      function Equality return Expr_Access is
         E : Expr_Access := Comparison;
      begin
         while Self.Match ((Bang_Equal_Token, Equal_Equal_Token)) loop
            declare
               Op    : constant Token       := Self.Previous;
               Right : constant Expr_Access := Comparison;
            begin
               E :=
                 new Expr'
                   (Kind => Binary_Kind_Type, Left => E, Right => Right,
                    Op   => Op);

            end;
         end loop;
         return E;
      end Equality;

      function And_Expression return Expr_Access is
         E : Expr_Access := Equality;
      begin
         while Self.Match ((1 => And_Token)) loop
            declare
               Op    : constant Token       := Self.Previous;
               Right : constant Expr_Access := Equality;
            begin
               E :=
                 new Expr'
                   (Kind => Logical_Kind_Type, Left => E, Right => Right,
                    Op   => Op);
            end;
         end loop;
         return E;
      end And_Expression;

      function Or_Expression return Expr_Access is
         E : Expr_Access := And_Expression;
      begin
         while Self.Match ((1 => Or_Token)) loop
            declare
               Op    : constant Token       := Self.Previous;
               Right : constant Expr_Access := And_Expression;
            begin
               E :=
                 new Expr'
                   (Kind => Logical_Kind_Type, Left => E, Right => Right,
                    Op   => Op);
            end;
         end loop;
         return E;
      end Or_Expression;

   begin
      return Or_Expression;
   end Expression;

   function Parse (Self : in out Parser) return Stmt_Vector is

      procedure Synchronize is
      begin
         Self.Skip;
         if Self.Previous.Kind = Semicolon_Token then
            return;
         end if;
         case Self.Peek.Kind is
            when Class_Token | Fun_Token | Var_Token | For_Token | If_Token
              | While_Token | Print_Token | Return_Token =>
               return;
            when others =>
               Self.Skip;
         end case;
      end Synchronize;

      function Statement return Stmt_Access is

         function Expression_Statement return Stmt_Access is
            E : constant Expr_Access := Self.Expression;
         begin
            Self.Consume_Skip
              (Semicolon_Token, "Expected ';' after expression.");
            return new Stmt'(Expression_Kind_Type, E);
         end Expression_Statement;

         function Print_Statement return Stmt_Access is
            Value : Expr_Access := Self.Expression;
         begin
            Self.Consume_Skip (Semicolon_Token, "Expected ';' after value.");
            return new Stmt'(Print_Stmt_Kind_Type, Value);

         end Print_Statement;

         function Var_Declaration return Stmt_Access is
            Name        : Token :=
              Self.Consume (Identifier_Token, "Expected variable name.");
            Initializer : Expr_Access;
         begin
            if Self.Match ((1 => Equal_Token)) then
               Initializer := Self.Expression;
            end if;
            Self.Consume_Skip
              (Semicolon_Token, "Expected ';' after variable declaration.");
            return
              new Stmt'
                (Kind        => Var_Decl_Stmt_Kind_Type, Name => Name,
                 Initializer => Initializer);
         end Var_Declaration;

         function Declaration return Stmt_Access is
         begin
            if Self.Match ((1 => Var_Token)) then
               return Var_Declaration;
            end if;
            return Statement;
         exception
            when Parser_Error =>
               Synchronize;
               return null;

         end Declaration;

      begin
         if Self.Match ((1 => Print_Token)) then
            return Print_Statement;
         end if;

         return Expression_Statement;

      end Statement;

      Stmts : Stmt_Vector;
   begin

      while not Self.Is_At_End loop
         Stmts.Append (Statement);
      end loop;
      return Stmts;
   exception
      when Parser_Error =>
         Synchronize;
         Put_Line ("Got Parser Error! Synchronizing");
         return Stmts;
   end Parse;

end Parsers;
