with Literals;  use Literals;
with Functions; use Functions;
package Lox_Primitives is
    type Lox_Primitive_Kind is
       (Literal_Result_Type, Function_Result_Type, Nothing_Primitive);
    --   type Literal (Kind : Literal_Kind := Nothing) is record
    --        case Kind is
    --           when Float_Type =>
    --              Float_Val : Float;
    --           when Int_Type =>
    --              Int_Val : Integer;
    --           when Bool_Type =>
    --              Bool_Val : Boolean;
    --           when String_Type =>
    --              String_Val : Lexeme_String;
    --           when Nothing =>
    --              null;
    --        end case;
    --     end record;

    type Lox_Primitive (Kind : Lox_Primitive_Kind := Nothing_Primitive) is record
        case Kind is
            when Literal_Result_Type =>
                Lit : Literal;
            when Function_Result_Type =>
                Callable : Lox_Callable;
            when Nothing_Primitive =>
                null;
        end case;
    end record;

    function Stringify (L : Lox_Primitive) return String;
    function Is_Truthy (L : Lox_Primitive) return Boolean;
    function Is_Equal (A : Lox_Primitive; B : Lox_Primitive) return Boolean;

end Lox_Primitives;
