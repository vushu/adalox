with AUnit.Assertions; use AUnit.Assertions;
with Lexeme_Strings;   use Lexeme_Strings;
--  with Ada.Text_IO;      use Ada.Text_IO;
package body Scanners.Test is

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Scanners package");
   end Name;

   procedure Test_Single_Tokens is
      TokenList    : Token_Vector;
      Input_String : constant String := "+-*/.{-";
      Current_Char : String (1 .. 1);
   begin
      TokenList := Scan_Tokens (Input_String);
      --  Minus 1 since we don't wan't to compare with
      --  End of file Token which is the last index
      for Index in 0 .. TokenList.Last_Index - 1 loop
         Current_Char := Input_String (Index + 1 .. Index + 1);
         Assert
           (To_String (TokenList (Index).Lexeme) = Current_Char,
            "Should be " & Current_Char);
      end loop;

   end Test_Single_Tokens;

   procedure Test_Handling_String is
      Input_String : constant String       := """Hej med dig""";
      TokenList    : constant Token_Vector := Scan_Tokens (Input_String);
      T            : constant Token        := TokenList (0);
   begin
      Assert (T.Lexeme = Input_String, "Should be the same");
   end Test_Handling_String;

 procedure Test_Handling_Number is
      Input_String : constant String       := "2.22.2";
      TokenList    : constant Token_Vector := Scan_Tokens (Input_String);
      T            : constant Token        := TokenList (0);
      T2            : constant Token        := TokenList (1);
      T3            : constant Token        := TokenList (2);
   begin
      Print_Lexeme (T);
      Print_Lexeme (T2);
      Print_Lexeme (T3);
      Assert (T.Lexeme = "2.22", "Should be the same");
      Assert (T2.Lexeme = ".", "Should be the same");
      Assert (T3.Lexeme = "2", "Should be the same");
   end Test_Handling_Number;


   procedure Test_Tab_String is
      Tab          : constant Character    := Character'Val (9);
      Input_String : constant String       := "" & Tab;
      TokenList    : constant Token_Vector := Scan_Tokens (Input_String);
      N_Elements   : constant Integer      := Integer (TokenList.Length);
   begin

      Assert (N_Elements = 1, "Should only have 1 token");
      Assert
        (TokenList (0).Kind = End_Of_File_Token,
         "Should be End_Of_File_Token");
   end Test_Tab_String;

   procedure Test_Handling_Identifier is
      Input_String : constant String       := "class";
      TokenList    : constant Token_Vector := Scan_Tokens (Input_String);
      T            : constant Token        := TokenList (0);
   begin
      Assert (T.Kind = Class_Token, "Should be the Class_Token");
   end Test_Handling_Identifier;

   overriding procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Test_Single_Tokens;
      Test_Handling_String;
      Test_Handling_Identifier;
      Test_Tab_String;
      Test_Handling_Number;
   end Run_Test;
end Scanners.Test;
