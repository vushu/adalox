with Tokens;         use Tokens;
with Scanners;       use Scanners;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

procedure Adalox is
   Tokens : Token_Vector;
begin
   Tokens := Scan_Tokens ("((()");

   Put_Line ("Vector has" & Count_Type'Image (Tokens.Length) & " elements");
end Adalox;
