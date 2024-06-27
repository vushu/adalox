with Ada.Text_IO;    use Ada.Text_IO;
with Lexeme_Strings; use Lexeme_Strings;
with Error_Reports;
package body Environments is

   procedure Define
     (Self : in out Environment; Name : Lexeme_String; Value : Literal)
   is
   begin
      Self.Values.Include (Name, Value);
   end Define;

   function Get (Self : in out Environment; Name : Token) return Literal is
      Runtime_Error : exception;
      Cursor : constant Environment_Values.Cursor :=
        Self.Values.Find (Name.Lexeme);
   begin
      if Environment_Values.Has_Element (Cursor) then
         return Environment_Values.Element (Cursor);
      end if;

      if Self.Enclosing /= null then
         return Self.Enclosing.Get (Name);
      end if;

      raise Runtime_Error
        with "Undefined variable '" & Name.Lexeme.To_String & "'.";
   end Get;

   procedure Assign (Self : in out Environment; Name : Token; Value : Literal)
   is
   begin

      if Self.Values.Contains (Name.Lexeme) then
         Self.Values.Include (Name.Lexeme, Value);
         return;
      end if;

      if Self.Enclosing /= null then
         Self.Enclosing.Assign (Name, Value);
         return;
      end if;

      raise Error_Reports.Runtime_Error
        with "Undefined variable '" & Name.Lexeme.To_String & "',";
   end Assign;

   function Make_Environment
     (Enclosing : Environment_Access) return Environment_Access
   is
      E : Environment_Access := new Environment;
   begin
      E.Enclosing := Enclosing;
      return E;
   end Make_Environment;
end Environments;
