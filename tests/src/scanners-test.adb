with AUnit.Assertions; use AUnit.Assertions;
with Scanners;         use Scanners;
with Ada.Text_IO; use Ada.Text_IO;
package body Scanners.Test is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Scanners package");
   end Name;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      TokenList : Token_Vector;
      Count : Integer;
   begin
      TokenList := Scan_Tokens ("())");
      Count := Integer(TokenList.Length);
      Assert (Count = 3, "Should be 3");
   end Run_Test;

end Scanners.Test;
