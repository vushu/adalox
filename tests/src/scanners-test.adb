with AUnit.Assertions; use AUnit.Assertions;
with Lexeme_Strings;   use Lexeme_Strings;
package body Scanners.Test is

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Scanners package");
   end Name;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      TokenList    : Token_Vector;
      Count        : Integer;
      Input_String : constant String := "+-*/.{-";
      Current_Char : String (1 .. 1);
   begin
      TokenList := Scan_Tokens (Input_String);
      Count     := Integer (TokenList.Length);
      for Index in 0 .. Count - 1 loop
         Current_Char := Input_String (Index + 1 .. Index + 1);
         Assert
           (To_String (TokenList (Index).Lexeme) = Current_Char,
            "Should be " & Current_Char);
      end loop;
   end Run_Test;

end Scanners.Test;
