with Tokens; use Tokens;
package Error_Reports is
    Had_Error         : Boolean := False;
    Had_Runtime_Error : Boolean := False;
    procedure Report
       (Line : Positive; Where : String; Message : String);
    procedure Error (T : in Token; Message : String);
end Error_Reports;
