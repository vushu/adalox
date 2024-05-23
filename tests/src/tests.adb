with AUnit.Reporter.Text;
with AUnit.Run;
with Scanners_Suites; use Scanners_Suites;
with Combined_Suites; use Combined_Suites;
with Parser_Suites;   use Parser_Suites;
procedure Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Scanner_Suite);
   procedure Runner2 is new AUnit.Run.Test_Runner (Combined_Suites.Suite);
   procedure Runner3 is new AUnit.Run.Test_Runner (Parser_Suites.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
   Runner2 (Reporter);
   Runner3 (Reporter);
end Tests;
