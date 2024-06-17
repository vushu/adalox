with Ada.Text_IO; use Ada.Text_IO;
package body Error_Reports is
   procedure Report (Line : Positive; Where : String; Message : String) is
   begin
      Put_Line ("[line] " & Line'Image & "] Error" & Where & ": " & Message);
      Had_Error := True;
   end Report;

   procedure Error (T : Token; Message : String) is
   begin
      if T.Kind = End_Of_File_Token then
         Report (T.Line, " at end", Message);
      else
         Report (T.Line, " at '" & T.Lexeme.To_String & "'", Message);
      end if;
   end Error;
end Error_Reports;
