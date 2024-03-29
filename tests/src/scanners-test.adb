with AUnit.Assertions; use AUnit.Assertions;
with Lexeme_Strings;   use Lexeme_Strings;
with Ada.Text_IO;      use Ada.Text_IO;
--  with Tokens;           use Tokens;
package body Scanners.Test is

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Scanners package");
   end Name;

   procedure Test_Single_Tokens is
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

   end Test_Single_Tokens;

   procedure Test_Handling_String is
      Input_String : constant String       := """Hej med dig""";
      TokenList    : constant Token_Vector := Scan_Tokens (Input_String);
      T            : Token;
   begin
      T := TokenList (0);
      Assert (T.Lexeme = Input_String, "Should be the same");
   end Test_Handling_String;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Test_Single_Tokens;
      Test_Handling_String;
   end Run_Test;
end Scanners.Test;
