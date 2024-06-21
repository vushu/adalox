with Ada.Text_IO;  use Ada.Text_IO;
with Interpreters; use Interpreters;
with Parsers;      use Parsers;
with Scanners;     use Scanners;
with Tokens;       use Tokens;
with AST;          use AST;
with Literals;     use Literals;
package body Interpreter_Test is

   procedure Print_Tokens (Tokens : Token_Vector) is
   begin
      for T of Tokens loop
         T.Print_Token;
      end loop;

   end Print_Tokens;

   procedure Test_Interpreter is
      S      : constant String       := "(2+2)*2 / 2 == 4 and 1 != 0;";
      Tokens : constant Token_Vector := Scan_Tokens (S);
      Stmts  : constant Stmt_Vector  := Parse (Tokens);
   begin
      Interpret (Stmts);
   end Test_Interpreter;

   procedure Test_Interpreter_Or_Logic is
      Tokens : constant Token_Vector := Scan_Tokens ("true or false;");
      Stmts  : constant Stmt_Vector  := Parse (Tokens);
   begin
      Interpret (Stmts);
   end Test_Interpreter_Or_Logic;

   procedure Test_Interpreter_And_Logic is
      Tokens : constant Token_Vector := Scan_Tokens ("true and false;");
      Stmts  : constant Stmt_Vector  := Parse (Tokens);
   begin
      Interpret (Stmts);
   end Test_Interpreter_And_Logic;

   procedure Test_Interpreter_Arithmetic is
      Tokens : constant Token_Vector := Scan_Tokens ("2 * 5 + 4;");
      Stmts  : constant Stmt_Vector  := Parse (Tokens);
   begin
      Interpret (Stmts);
   end Test_Interpreter_Arithmetic;


   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Interpreter package");
   end Name;
   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Test_Interpreter;
      Test_Interpreter_Or_Logic;
      Test_Interpreter_And_Logic;
      Test_Interpreter_Arithmetic;
   end Run_Test;

end Interpreter_Test;
