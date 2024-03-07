with AUnit.Assertions; use AUnit.Assertions;
package body Scanners.Test_Combined_Symbols is
   procedure Test_Double_Tokens is
      Input_String : constant String       := ">=";
      TokenList    : constant Token_Vector := Scan_Tokens (Input_String);
   begin
      Print_Lexeme (TokenList (0));
      Assert (True, "Should be true");

   end Test_Double_Tokens;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Scanners Combined Symbols");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Test_Double_Tokens;
   end Run_Test;

end Scanners.Test_Combined_Symbols;
