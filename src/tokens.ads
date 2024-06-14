with Lexeme_Strings; use Lexeme_Strings;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Literals;       use Literals;
package Tokens is

   type Token_Kind is
     (Left_Paren_Token, Right_Paren_Token, Left_Brace_Token, Right_Brace_Token,
      Comma_Token, Dot_Token, Minus_Token, Plus_Token, Semicolon_Token,
      Slash_Token, Star_Token, Bang_Token, Bang_Equal_Token, Equal_Token,
      Equal_Equal_Token, Greater_Token, Greater_Equal_Token, Less_Token,
      Less_Equal_Token, Identifier_Token, String_Token, Number_Token,
      And_Token, Or_Token, Class_Token, Else_Token, False_Token, Fun_Token,
      For_Token, If_Token, Nil_Token, Print_Token, Return_Token, Super_Token,
      This_Token, True_Token, Var_Token, While_Token, End_Of_File_Token);

   type Token is tagged record
      Kind          : Token_Kind;
      Lexeme        : Lexeme_String;
      Literal_Value : Literal;
      Line          : Positive;
   end record;

   procedure Print_Lexeme (T : Token);
   procedure Print_Token (T : Token);
   function Create_Token
     (TK : Token_Kind; S : String; L : Literal) return Token;

   function Create_EOF_Token return Token;

   package Token_list is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Token);
   subtype Token_Vector is Token_list.Vector;

   function Lexeme_String_Hash
     (LS : Lexeme_String) return Ada.Containers.Hash_Type;

   package Keywords is new Ada.Containers.Hashed_Maps
     (Key_Type => Lexeme_String, Element_Type => Token_Kind,
      Hash     => Lexeme_String_Hash, Equivalent_Keys => "=");

end Tokens;
