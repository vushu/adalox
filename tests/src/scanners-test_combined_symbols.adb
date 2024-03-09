with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers;   use Ada.Containers;
with Ada.Text_IO;      use Ada.Text_IO;
with Lexeme_Strings;   use Lexeme_Strings;
package body Scanners.Test_Combined_Symbols is
   procedure Test_Double_Tokens is
      Input_String : constant String       := ">===!=<=";
      TokenList    : constant Token_Vector := Scan_Tokens (Input_String);
   begin
      for I in 0 .. TokenList.Last_Index loop
         declare
            T : Token           := TokenList (I);
            C : String (1 .. 2) := Input_String (I + I + 1 .. I + I + 2);
         begin
            Assert (T.Lexeme = C, "Should match");
         end;
      end loop;

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
