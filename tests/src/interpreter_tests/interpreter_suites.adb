with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Interpreter_Test;
package body Interpreter_Suites is
   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin

      Ret.Add_Test (Test_Case_Access'(new Interpreter_Test.Test));
      return Ret;
   end Suite;

end Interpreter_Suites;
