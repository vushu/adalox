with Error_Reports;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Float_Text_IO;
with AST;            use AST;
with Tokens;         use Tokens;
with Lexeme_Strings; use Lexeme_Strings;
with Lox_Functions;  use Lox_Functions;
with Lox_Primitives; use Lox_Primitives;
package body Interpreters is

   procedure Interpret (Statements : AST.Stmt_Vector) is
   begin
      Env := new Environment;
      for Statement of Statements loop
         Execute (Statement);
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
      if Left.Kind = Float_Type and then Right.Kind = Float_Type then
         return;
      end if;

      raise Runtime_Error with "Operands must be numbers";
   end Check_Number_Operands;

   function Evaluate_Expr (E : Expr_Access) return Lox_Primitive is
   begin
      case E.Kind is
         when Unary_Kind_Type =>
            return Evaluate_Unary_Expr (E);
         when Variable_Kind_Type =>
            return Evaluate_Variable_Expr (E);
         when Assign_Kind_Type =>
            return Evaluate_Assign_Expr (E);
         when Grouping_Kind_Type =>
            return Evaluate_Expr (E.Expression);
         when Logical_Kind_Type =>
            return Evaluate_Logical_Expr (E);
         when Binary_Kind_Type =>
            return Evaluate_Binary_Expr (E);
         when Literal_Kind_Type =>
            return (Literal_Result_Type, E.Value);
         when others =>
            return (Kind => Nothing_Primitive);
      end case;
   end Evaluate_Expr;

   function Evaluate_Logical_Expr (E : Expr_Access) return Lox_Primitive is
      Left : constant Lox_Primitive := Evaluate_Expr (E.Left);
   begin
      if E.Op.Kind = Or_Token and then Is_Truthy (Left) then
         return Left;
      elsif E.Op.Kind = And_Token and then not Is_Truthy (Left) then
         return Left;
      end if;
      return Evaluate_Expr (E.Right);
   end Evaluate_Logical_Expr;

   function Evaluate_Binary_Expr (E : Expr_Access) return Lox_Primitive is
      Left  : constant Lox_Primitive := Evaluate_Expr (E.Left);
      Right : constant Lox_Primitive := Evaluate_Expr (E.Right);
   begin
      case E.Op.Kind is
         when Minus_Token =>
            Check_Number_Operands (E.Op, Left.Lit, Right.Lit);
            return
              (Literal_Result_Type,
               (Float_Type,
                Float_Val => Left.Lit.Float_Val + Right.Lit.Float_Val));
         when Slash_Token =>
            Check_Number_Operands (E.Op, Left.Lit, Right.Lit);
            return
              (Literal_Result_Type,
               (Float_Type,
                Float_Val => Left.Lit.Float_Val / Right.Lit.Float_Val));
         when Star_Token =>
            Check_Number_Operands (E.Op, Left.Lit, Right.Lit);
            return
              (Literal_Result_Type,
               (Float_Type,
                Float_Val => Left.Lit.Float_Val * Right.Lit.Float_Val));
         when Plus_Token =>
            if Left.Lit.Kind = Float_Type and then Right.Lit.Kind = Float_Type
            then
               return
                 Lox_Primitive'
                   (Literal_Result_Type,
                    Literal'
                      (Float_Type, Left.Lit.Float_Val + Right.Lit.Float_Val));
            elsif Left.Lit.Kind = String_Type
              and then Right.Lit.Kind = String_Type
            then
               return
                 Lox_Primitive'
                   (Literal_Result_Type,
                    (String_Type,
                     Make_Lexeme_String
                       (Left.Lit.String_Val.To_String &
                        Right.Lit.String_Val.To_String)));
            end if;
            raise Runtime_Error
              with "Operands must be two numbers or two strings.";
         when Greater_Token =>
            return
              (Literal_Result_Type,
               (Bool_Type, Left.Lit.Float_Val > Right.Lit.Float_Val));
         when Greater_Equal_Token =>
            return
              (Literal_Result_Type,
               (Bool_Type, Left.Lit.Float_Val >= Right.Lit.Float_Val));
         when Bang_Equal_Token =>
            return
              (Literal_Result_Type, (Bool_Type, not Is_Equal (Left, Right)));
         when Equal_Equal_Token =>
            return (Literal_Result_Type, (Bool_Type, Is_Equal (Left, Right)));
         when Less_Token =>
            return
              (Literal_Result_Type,
               (Bool_Type, Left.Lit.Float_Val < Right.Lit.Float_Val));
         when Less_Equal_Token =>
            return
              (Literal_Result_Type,
               (Bool_Type, Left.Lit.Float_Val <= Right.Lit.Float_Val));
         when others =>
            null;
      end case;
      raise Runtime_Error
        with "Unknown binary operator: " & E.Op.Lexeme.To_String;
   end Evaluate_Binary_Expr;

   function Evaluate_Unary_Expr (E : Expr_Access) return Lox_Primitive is
      Right : Lox_Primitive := Evaluate_Expr (E.Unary_Right);
   begin
      case E.Unary_Op.Kind is
         when Bang_Token =>
            return (Literal_Result_Type, (Bool_Type, not Is_Truthy (Right)));
         when Minus_Token =>
            Check_Number_Operand (E.Unary_Op, Right.Lit);
            return (Literal_Result_Type, (Float_Type, -Right.Lit.Float_Val));
         when others =>
            return (Kind => Nothing_Primitive);
      end case;

   end Evaluate_Unary_Expr;

   function Evaluate_Grouping_Expr (E : Expr_Access) return Lox_Primitive is
   begin
      return Evaluate_Expr (E);
   end Evaluate_Grouping_Expr;

   function Evaluate_Variable_Expr (E : Expr_Access) return Lox_Primitive is
   begin
      return Env.Get (E.Variable_Name);
   end Evaluate_Variable_Expr;

   function Evaluate_Assign_Expr (E : Expr_Access) return Lox_Primitive is
      Value : Lox_Primitive := Evaluate_Expr (E.Assign_Value);
   begin
      Env.Assign (E.Assign_Name, Value);
      return Value;
   end Evaluate_Assign_Expr;

   function Evaluate_Call_Expr (E : Expr_Access) return Lox_Primitive is
      Callee    : Lox_Primitive   := Evaluate_Expr (E.Callee);
      Arguments : Lox_Primitive_Vector;
      Func      : Function_Access := Callee.Callable;
   begin
      for Arg of E.Arguments loop
         Arguments.Append (Evaluate_Expr (Arg));
      end loop;
      --  return Func.Call (Arguments);
      return (Kind => Nothing_Primitive);

   end Evaluate_Call_Expr;

   procedure Evaluate_Print_Stmt (S : Stmt_Access) is
      Value : Lox_Primitive := Evaluate_Expr (S.Expression);
   begin
      Put_Line (Stringify (Value));
   end Evaluate_Print_Stmt;

   procedure Evaluate_Expression_Stmt (S : Stmt_Access) is
      Res : Lox_Primitive;
   begin
      Res := Evaluate_Expr (S.Expression);
   end Evaluate_Expression_Stmt;

   procedure Evaluate_Var_Decl_Stmt (S : Stmt_Access) is
      Value : Lox_Primitive := (Kind => Nothing_Primitive);
   begin
      if S.Initializer /= null then
         Value := Evaluate_Expr (S.Initializer);
      end if;
      Env.Define (S.Name.Lexeme, Value);
   end Evaluate_Var_Decl_Stmt;

   procedure Evaluate_If_Stmt (S : Stmt_Access) is
      Cond : Lox_Primitive := Evaluate_Expr (S.If_Condition);
   begin
      if Is_Truthy (Cond) then
         Execute (S.If_Then_Branch);
      elsif S.If_Else_Branch /= null then
         Execute (S.If_Else_Branch);
      end if;
   end Evaluate_If_Stmt;

   procedure Evaluate_While_Stmt (S : Stmt_Access) is
   begin
      while Is_Truthy (Evaluate_Expr (S.While_Condition)) loop
         Execute (S.While_Body);
      end loop;
   end Evaluate_While_Stmt;

   procedure Execute_Block (Statements : Stmt_Vector; E : Environment_Access)
   is
      Prev : Environment_Access := Env;
   begin
      Env := E;
      for Stmt of Statements loop
         Execute (Stmt);
      end loop;
      Env := Prev;
   exception
      when Error_Reports.Runtime_Error =>
         Put_Line ("Execute_Block failed!");
         Env := Prev;
      --  when others                      =>
      --     Put_Line ("Execute_Block failed for others!");
      --     Env := Prev;

   end Execute_Block;

   procedure Evaluate_Block_Stmt (S : Stmt_Access) is
   begin
      Execute_Block (S.Block_Statements, Make_Environment (Env));
   end Evaluate_Block_Stmt;

   procedure Execute (Stmt : Stmt_Access) is
   begin
      case Stmt.Kind is
         when Expression_Kind_Type =>
            Evaluate_Expression_Stmt (Stmt);
         when Print_Stmt_Kind_Type =>
            Evaluate_Print_Stmt (Stmt);
         when Var_Decl_Stmt_Kind_Type =>
            Evaluate_Var_Decl_Stmt (Stmt);
         when Block_Kind_Type =>
            Evaluate_Block_Stmt (Stmt);
         when If_Stmt_Kind_Type =>
            Evaluate_If_Stmt (Stmt);
         when While_Stmt_Kind_Type =>
            Evaluate_While_Stmt (Stmt);
      end case;
   end Execute;

end Interpreters;
