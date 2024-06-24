with Error_Reports;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Float_Text_IO;
with AST;            use AST;
with Tokens;         use Tokens;
with Lexeme_Strings; use Lexeme_Strings;
package body Interpreters is

   procedure Interpret (Statements : AST.Stmt_Vector) is

   begin
      for Statement of Statements loop
         Put_Line (Stringify (Evaluate_Expr (Statement.Expression)));
      end loop;
      --  exception
      --     when Runtime_Error =>
      --        Put_Line ("Runtime Error occured!");
   end Interpret;

   procedure Check_Number_Operand (T : Tokens.Token; Operand : Literal) is
   begin
      if Operand.Kind = Float_Type then
         return;
      end if;
      raise Runtime_Error
        with T.Literal_Value.String_Val.To_String & " Operand must be number";
   end Check_Number_Operand;

   procedure Check_Number_Operands
     (T : Tokens.Token; Left : Literal; Right : Literal)
   is
   begin
      if Left.Kind = Float_Type and Right.Kind = Float_Type then
         return;
      end if;

      raise Runtime_Error with "Operands must be numbers";
   end Check_Number_Operands;

   function Evaluate_Expr (E : Expr_Access) return Literal is
   begin
      case E.Kind is
         when Unary_Kind_Type =>
            return Evaluate_Unary_Expr (E);
         when Variable_Kind_Type =>
            return (Kind => Nothing);
         when Grouping_Kind_Type =>
            return Evaluate_Expr (E.Expression);
         when Logical_Kind_Type =>
            return Evaluate_Logical_Expr (E);
         when Binary_Kind_Type =>
            return Evaluate_Binary_Expr (E);
         when Literal_Kind_Type =>
            return E.Value;
         when others =>
            return (Kind => Nothing);
      end case;
   end Evaluate_Expr;

   function Evaluate_Logical_Expr (E : Expr_Access) return Literal is
      Left : constant Literal := Evaluate_Expr (E.Left);
   begin
      if E.Op.Kind = Or_Token then
         if Is_Truthy (Left) then
            return Left;
         else
            if not Is_Truthy (Left) then
               return Left;
            end if;
         end if;
      end if;
      return Evaluate_Expr (E.Right);
   end Evaluate_Logical_Expr;

   function Evaluate_Binary_Expr (E : Expr_Access) return Literal is
      Left  : constant Literal := Evaluate_Expr (E.Left);
      Right : constant Literal := Evaluate_Expr (E.Right);
   begin
      case E.Op.Kind is
         when Minus_Token =>
            Check_Number_Operands (E.Op, Left, Right);
            return (Float_Type, Float_Val => Left.Float_Val + Right.Float_Val);
         when Slash_Token =>
            Check_Number_Operands (E.Op, Left, Right);
            return (Float_Type, Float_Val => Left.Float_Val / Right.Float_Val);
         when Star_Token =>
            Check_Number_Operands (E.Op, Left, Right);
            return (Float_Type, Float_Val => Left.Float_Val * Right.Float_Val);
         when Plus_Token =>
            if Left.Kind = Float_Type and then Right.Kind = Float_Type then
               return Literal'(Float_Type, Left.Float_Val + Right.Float_Val);
            elsif Left.Kind = String_Type and then Right.Kind = String_Type
            then
               return
                 (String_Type,
                  Make_Lexeme_String
                    (Left.String_Val.To_String & Right.String_Val.To_String));
            end if;
            raise Runtime_Error
              with "Operands must be two numbers or two strings.";
         when Greater_Token =>
            return (Bool_Type, Left.Float_Val > Right.Float_Val);
         when Greater_Equal_Token =>
            return (Bool_Type, Left.Float_Val >= Right.Float_Val);
         when Bang_Equal_Token =>
            return (Bool_Type, not Is_Equal (Left, Right));
         when Equal_Equal_Token =>
            return (Bool_Type, Is_Equal (Left, Right));
         when others =>
            null;
      end case;
      raise Runtime_Error
        with "Unknown binary operator: " & E.Op.Lexeme.To_String;
   end Evaluate_Binary_Expr;

   function Evaluate_Unary_Expr (E : Expr_Access) return Literal is
      Right : Literal := Evaluate_Expr (E.Unary_Right);
   begin
      case E.Unary_Op.Kind is
         when Bang_Token =>
            return (Bool_Type, not Is_Truthy (Right));
         when Minus_Token =>
            Check_Number_Operand (E.Unary_Op, Right);
            return (Float_Type, -Right.Float_Val);
         when others =>
            return (Kind => Nothing);
      end case;

   end Evaluate_Unary_Expr;

   function Evaluate_Grouping_Expr (E : Expr_Access) return Literal is
   begin
      return Evaluate_Expr (E);
   end Evaluate_Grouping_Expr;

end Interpreters;
