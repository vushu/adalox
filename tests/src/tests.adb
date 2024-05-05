with AUnit.Reporter.Text;
with AUnit.Run;
with Scanners_Suites; use Scanners_Suites;
with Combined_Suites; use Combined_Suites;
procedure Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Scanner_Suite);
   procedure Runner2 is new AUnit.Run.Test_Runner (Combined_Suites.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
   Runner2 (Reporter);
end Tests;
