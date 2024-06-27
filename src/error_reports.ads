with Tokens; use Tokens;
package Error_Reports is
   Runtime_Error : exception;
   Had_Error         : Boolean := False;
   Had_Runtime_Error : Boolean := False;
   procedure Report (Line : Positive; Where : String; Message : String);
   procedure Error (T : Token; Message : String);
end Error_Reports;
