with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Parser_Test;
package body Parser_Suites is
   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Test_Case_Access'(new Parser_Test.Test));
      return Ret;
   end Suite;

end Parser_Suites;
