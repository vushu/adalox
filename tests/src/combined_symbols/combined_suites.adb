with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with Scanners.Test_Combined_Symbols;
package body Combined_Suites is
   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Test_Case_Access'(new Scanners.Test_Combined_Symbols.Test));
      return Ret;
   end Suite;

end Combined_Suites;
