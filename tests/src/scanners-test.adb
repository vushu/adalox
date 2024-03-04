with AUnit.Assertions; use AUnit.Assertions;
package body Scanners.Test is

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Scanners package");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      TokenList : Token_Vector;
      Count     : Integer;
   begin
      TokenList := Scan_Tokens ("())");
      Count     := Integer (TokenList.Length);
      Assert (Count = 3, "Should not be 3");
   end Run_Test;

end Scanners.Test;
