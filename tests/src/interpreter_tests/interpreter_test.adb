with Ada.Text_IO;  use Ada.Text_IO;
with Interpreters; use Interpreters;
with Parsers;      use Parsers;
with Scanners;     use Scanners;
package body Interpreter_Test is

   procedure Test_Interpreter is
   begin
      Put_Line ("Testing Interpreter");
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
