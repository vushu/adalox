with Ada.Float_Text_IO;
package body Lox_Primitives is
   function Stringify (L : Lox_Primitive) return String is
   begin
      case L.Lit.Kind is
         when Nothing =>
            return "nil";

         when Float_Type =>
            declare
               Number_Text : String := L.Lit.Float_Val'Image;
               Last_Text   : String :=
                 Number_Text ((Number_Text'Last - 1) .. Number_Text'Last);
            begin
               if Last_Text = ".0" then
                  Number_Text := Number_Text (1 .. Number_Text'Last - 2);
               end if;
               Ada.Float_Text_IO.Put
                 (To  => Number_Text, Item => Float'Value (Number_Text),
                  Aft => 3, Exp => 0);
               return Number_Text;
            end;
         when Bool_Type =>
            return L.Lit.Bool_Val'Image;
         when String_Type =>
            return L.Lit.String_Val.To_String;
         when others =>
            return "";
      end case;
   end Stringify;

   function Is_Truthy (L : Lox_Primitive) return Boolean is
   begin
      case L.Lit.Kind is
         when Nothing =>
            return False;
         when Bool_Type =>
            return L.Lit.Bool_Val;
         when others =>
            return True;
      end case;
   end Is_Truthy;

   function Is_Equal (A : Lox_Primitive; B : Lox_Primitive) return Boolean is
   begin
      if A.Lit.Kind = Nothing and B.Lit.Kind = Nothing then
         return True;
      end if;

      if A.Lit.Kind = Nothing then
         return False;
      end if;
      return A.Lit = B.Lit;
   end Is_Equal;
end Lox_Primitives;
