with Literals; use Literals;
with Ada.Containers.Vectors;
limited with Lox_Functions;

package Lox_Primitives is

    type Lox_Primitive_Kind is
       (Literal_Result_Type, Function_Result_Type, Nothing_Primitive);
    type Function_Access is access Lox_Functions.Lox_Callable;
    type Lox_Primitive (Kind : Lox_Primitive_Kind := Nothing_Primitive) is
    record
        case Kind is
            when Literal_Result_Type =>
                Lit : Literal;
            when Function_Result_Type =>
                Callable : Function_Access;
            when Nothing_Primitive =>
                null;
        end case;
    end record;

    package lox_primitive_list is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => Lox_Primitive);
    subtype lox_primitive_vector is lox_primitive_list.Vector;

    function Stringify (L : Lox_Primitive) return String;
    function Is_Truthy (L : Lox_Primitive) return Boolean;
    function Is_Equal (A : Lox_Primitive; B : Lox_Primitive) return Boolean;

end Lox_Primitives;
