with Literals;       use Literals;
with Lexeme_Strings; use Lexeme_Strings;
with Tokens;         use Tokens;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;

package Environments is
    type Environment;
    type Environment_Access is access Environment;
    package Environment_Values is new Ada.Containers.Hashed_Maps
       (Key_Type => Lexeme_String, Element_Type => Literal,
        Hash     => Lexeme_String_Hash, Equivalent_Keys => "=");

    type Environment is tagged record
        Enclosing : Environment_Access;
        Values    : Environment_Values.Map;

    end record;

    procedure Define
       (Self : in out Environment; Name : Lexeme_String; Value : Literal);

    function Get (Self : in out Environment; Name : Token) return Literal;
    procedure Assign
       (Self : in out Environment; Name : Token; Value : Literal);
    function Make_Environment
       (Enclosing : Environment_Access) return Environment_Access;

end Environments;
