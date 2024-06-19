with Ada.Text_IO;  use Ada.Text_IO;
with Interpreters; use Interpreters;
with Parsers;      use Parsers;
with Scanners;     use Scanners;
with Tokens;       use Tokens;
with AST;          use AST;
with Literals;     use Literals;
package body Interpreter_Test is

   procedure Test_Interpreter is
      --  Input_String : constant String := "+-*/.{-";
      S      : constant String       := "(2+2)*2 / 2 == 4;";
      Tokens : constant Token_Vector := Scan_Tokens (S);
      Stmts  : constant Stmt_Vector  := Parse (Tokens);
   begin
      Interpret (Stmts);
   end Test_Interpreter;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Interpreter package");
   end Name;
   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Test_Interpreter;
   end Run_Test;

end Interpreter_Test;
