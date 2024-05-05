with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Scanners.Test;
package body Scanners_Suites is
   function Scanner_Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Test_Case_Access'(new Scanners.Test.Test));
      --  Ret.Add_Test
      --    (Test_Case_Access'(new Scanners.Test_Combined_Symbols.Test));
      return Ret;
   end Scanner_Suite;

end Scanners_Suites;
