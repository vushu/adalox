package Lexeme_Strings with SPARK_Mode is
   Max : constant Integer := 255;
   type Lexeme_String is private;

   function Make_Lexeme_String (Source : String) return Lexeme_String;
   function To_String (Source : Lexeme_String) return String;
private
   type Length_T is new Integer range 0 .. Max;
   type Lexeme_String is record
      Data   : String (1 .. Max);
      Length : Length_T;
   end record;
end Lexeme_Strings;
