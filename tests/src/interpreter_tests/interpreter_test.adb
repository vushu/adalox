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
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);
   end Test_Interpreter;
   procedure Test_Interpreter_Or_Logic is
      Tokens : constant Token_Vector :=
        Scan_Tokens ("true or false; print nil or ""yes"";");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      -- Should print yes
      Interpret (Stmts);
   end Test_Interpreter_Or_Logic;

   procedure Test_Interpreter_And_Logic is
      Tokens : constant Token_Vector := Scan_Tokens ("true and false;");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);
   end Test_Interpreter_And_Logic;

   procedure Test_Interpreter_Arithmetic is
      Tokens : constant Token_Vector := Scan_Tokens ("2 * 5 + 4;");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);
   end Test_Interpreter_Arithmetic;

   procedure Test_Interpreter_Plus is
      Tokens : constant Token_Vector := Scan_Tokens ("print 5 + 5;");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);
   end Test_Interpreter_Plus;

   procedure Test_Interpreter_Variable is
      Tokens : constant Token_Vector :=
        Scan_Tokens ("var a = 1; var b = 2; print a + b;");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);

   end Test_Interpreter_Variable;

   procedure Test_Interpreter_Assign is
      Tokens : constant Token_Vector :=
        Scan_Tokens ("var a = 1; print a = 2 + 1;");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);

   end Test_Interpreter_Assign;

   procedure Test_Interpreter_Scope is
      -- Should print out:
      --  inner a
      --  outer b
      --  global c
      --  outer a
      --  outer b
      --  global c
      --  global a
      --  global b
      --  global c
      Tokens : constant Token_Vector :=
        Scan_Tokens
          ("var a = ""global a""; var b = ""global b""; var c = ""global c"";{" &
           "var a = ""outer a""; var b = ""outer b"";{" &
           "var a = ""inner a""; print a; print b; print c; }" &
           "print a; print b; print c;} print a; print b; print c; print ""sdfasdf"";");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);
   end Test_Interpreter_Scope;

   procedure Test_Interpreter_If_Statement is
      Tokens : constant Token_Vector :=
        Scan_Tokens
          ("if (1 > 1) { print ""Hej med dig!"";} else { print ""Hej hej! "";}");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);

   end Test_Interpreter_If_Statement;

   procedure Test_Interpreter_While_Statement is
      Tokens : constant Token_Vector :=
        Scan_Tokens ("var i = 0; while (i <= 10) { print i ; i; i = i + 1;}");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);
   end Test_Interpreter_While_Statement;

   procedure Test_Interpreter_For_Statement is
      Tokens : constant Token_Vector :=
        Scan_Tokens
          ("var a = 0; var temp; for (var b = 1; a < 10000; b = temp + b) { print a; temp = a; a = b; }");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);

   end Test_Interpreter_For_Statement;

   procedure Test_Interpreter_Clock_Function_Call is
      Tokens : constant Token_Vector :=
        Scan_Tokens
          ("clock();");
      P      : Parser                := (0, Tokens);
      Stmts  : constant Stmt_Vector  := P.Parse;
   begin
      Interpret (Stmts);

   end Test_Interpreter_Clock_Function_Call;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Interpreter package");
   end Name;
   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Test_Interpreter;
      --  Test_Interpreter_Or_Logic;
      --  Test_Interpreter_And_Logic;
      --  Test_Interpreter_Arithmetic;
      --  Test_Interpreter_Plus;
      --  Test_Interpreter_Variable;
      --  Test_Interpreter_Assign;
      --  Test_Interpreter_Scope;
      --  Test_Interpreter_If_Statement;
      --  Test_Interpreter_While_Statement;
      --  Test_Interpreter_For_Statement;
      Test_Interpreter_Clock_Function_Call;
   end Run_Test;

end Interpreter_Test;
