with Ada.Containers.Hashed_Maps;
package Lexeme_Strings with
  SPARK_Mode
is
   Max : constant Integer := 255;
   type Length_T is new Integer range 0 .. Max;
   type Lexeme_String is tagged record
      Data   : String (1 .. Max);
      Length : Length_T;
   end record;
   function Make_Lexeme_String (Source : String) return Lexeme_String;
   function To_String (Source : Lexeme_String) return String;
   function "=" (Left : Lexeme_String; Right : String) return Boolean with
     Global => null;

   function Lexeme_String_Hash
     (LS : Lexeme_String) return Ada.Containers.Hash_Type;
   --  private
   --  type Length_T is new Integer range 0 .. Max;
end Lexeme_Strings;
