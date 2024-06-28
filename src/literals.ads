with Lexeme_Strings; use Lexeme_Strings;
with Ada.Text_IO; use Ada.Text_IO;

package Literals is
   type Literal_Kind is
     (Float_Type, Int_Type, Bool_Type, String_Type, Nothing);

   type Literal (Kind : Literal_Kind := Nothing) is record
      case Kind is
         when Float_Type =>
            Float_Val : Float;
         when Int_Type =>
            Int_Val : Integer;
         when Bool_Type =>
            Bool_Val : Boolean;
         when String_Type =>
            String_Val : Lexeme_String;
         when Nothing =>
            null;
      end case;
   end record;

   function Stringify (L : Literal) return String;
   function Is_Truthy (L : Literal) return Boolean;
   function Is_Equal (A : Literal; B : Literal) return Boolean;
end Literals;
