with Lox_Functions;  use Lox_Functions;
with Lox_Primitives; use Lox_Primitives;
with Literals;       use Literals;
with Ada.Calendar;
with Ada.Text_IO;    use Ada.Text_IO;

package body Lox_Native_Functions is
   overriding function Arity (Self : Clock_Function) return Natural is
   begin
      return 0;
   end Arity;

   overriding function Call
     (Self : Clock_Function; Arguments : Lox_Primitives.Lox_Primitive_Vector)
      return Lox_Primitives.Lox_Primitive
   is
      use type Ada.Calendar.Time;
      Current_Time : Ada.Calendar.Time := Ada.Calendar.Clock;

      Milliseconds : Duration;
      Seconds      : Float;

   begin
      Milliseconds :=
        Duration
          (Ada.Calendar."-"
             (Current_Time, Ada.Calendar.Time_Of (1_970, 1, 1)));
      Seconds      := Float (Milliseconds) / 1_000.0;
      Put_Line ("Clock says: " & Seconds'Image);

      return (Literal_Result_Type, (Float_Type, Seconds));
   end Call;

end Lox_Native_Functions;
