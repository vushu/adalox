with Lox_Functions; use Lox_Functions;
with Lox_Primitives;
package Lox_Native_Functions is

   type Clock_Function is new Lox_Functions.Lox_Callable with null record;

   overriding function Arity (Self : Clock_Function) return Natural;

   overriding function Call
     (Self : Clock_Function; Arguments : Lox_Primitives.Lox_Primitive_Vector)
      return Lox_Primitives.Lox_Primitive;

end Lox_Native_Functions;
