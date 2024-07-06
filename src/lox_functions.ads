--  with Interpreters; use Interpreters;
--  with Ada.Containers.Vectors;
--  limited with Lox_Primitives;
with Lox_Primitives;
package Lox_Functions is
   type Lox_Callable is interface;
   --  type Lox_Primitive_Acesss is access Lox_Primitives.Lox_Primitive;

   --  package Lox_Primitive_List is new Ada.Containers.Vectors
   --    (Index_Type => Natural, Element_Type => Lox_Primitive_Acesss);
   --  subtype Lox_Primitive_Vector is Lox_Primitive_List.Vector;

   function Call
     (Self : Lox_Callable; Arguments : Lox_Primitives.Lox_Primitive_Vector)
      return Lox_Primitives.Lox_Primitive is abstract;

end Lox_Functions;
