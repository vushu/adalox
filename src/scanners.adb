with Ada.Text_IO; use Ada.Text_IO;
package body Scanners is
   procedure Do_Nothing is
   begin
      Put_Line ("Item : String");
   end Do_Nothing;

   procedure Scan_Tokens (Source : String; Token_List : in out Token_Vector) is
   begin
      Put_Line ("Doing stuff to in Scan_Tokens");
   end Scan_Tokens;

end Scanners;
