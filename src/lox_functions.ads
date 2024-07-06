with Lox_Primitives;
package Lox_Functions is
   type Lox_Callable is interface;

   function Arity (Self : Lox_Callable) return Natural is abstract;

   function Call
     (Self : Lox_Callable; Arguments : Lox_Primitives.lox_primitive_vector)
      return Lox_Primitives.Lox_Primitive is abstract;

end Lox_Functions;
