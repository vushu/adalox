with Error_Reports;
with Ada.Text_IO; use Ada.Text_IO;
package body Interpreters is

   procedure Interpret (Statements : AST.Stmt_Vector) is
      Runtime_Error : exception;
      --  procedure Execute(Statement : Stmt_Access) is
      --  begin

      --  end Execute;
   begin
      for Statement of Statements loop
         Put_Line (Statement.Kind'Image);
      end loop;
   exception
      when Runtime_Error =>
         Put_Line ("Item : String");
   end Interpret;
end Interpreters;
