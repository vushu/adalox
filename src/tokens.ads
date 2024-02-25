with Lexeme_Strings; use Lexeme_Strings;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
package Tokens is

   type Token_Kind is
     (TOK_LEFT_PAREN, TOK_RIGHT_PAREN, TOK_LEFT_BRACE, TOK_RIGHT_BRACE,
      TOK_COMMA, TOK_DOT, TOK_MINUS, TOK_PLUS, TOK_SEMICOLON, TOK_SLASH,
      TOK_STAR, TOK_BANG, TOK_BANG_EQUAL, TOK_EQUAL, TOK_EQUAL_EQUAL,
      TOK_GREATER, TOK_GREATER_EQUAL, TOK_LESS, TOK_LESS_EQUAL, TOK_IDENTIFIER,
      TOK_STRING, TOK_NUMBER, TOK_AND, TOK_OR, TOK_CLASS, TOK_ELSE, TOK_FALSE,
      TOK_FUN, TOK_FOR, TOK_IF, TOK_NIL, TOK_PRINT, TOK_RETURN, TOK_SUPER,
      TOK_THIS, TOK_TRUE, TOK_VAR, TOK_WHILE, TOK_EOF);

   type Token is record
      Kind   : Token_Kind;
      Lexeme : Lexeme_String;
      Line   : Positive;
   end record;

   procedure Print_Lexeme (T : Token);
   procedure Print_Token (T : Token);
   function Create_Token (TK : Token_Kind) return Token;
   function Create_Token (TK : Token_Kind; S : String) return Token;

   package Token_list is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Token);
   subtype Token_Vector is Token_list.Vector;

end Tokens;
