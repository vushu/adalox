with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
package body Scanners is
   LF  : constant Character := Ada.Characters.Latin_1.LF;
   NUL : constant Character := Ada.Characters.Latin_1.NUL;
   CR  : constant Character := Ada.Characters.Latin_1.CR;
   HT  : constant Character := Ada.Characters.Latin_1.HT;

   function Is_At_End return Boolean is
   begin
      return Current >= Source_Size;
   end Is_At_End;
   function Match (Expected : Character) return Boolean is
   begin
      if Is_At_End then
         return False;
      end if;

      if Source (Current) /= Expected then
         return False;
      end if;

      Current := Current + 1;
      return True;
   end Match;

   function Peek return Character is
   begin
      if Is_At_End then
         return NUL;
      end if;
      return Source (Current);
   end Peek;

   function Advance return Character is
      C : Character;
   begin
      C       := Source (Current);
      Current := Current + 1;
      return C;
   end Advance;

   function Scan_Tokens (Src : String) return Token_Vector is
   begin
      Source_Size                    := Src'Last;
      Source (Src'First .. Src'Last) := Src;
      while not Is_At_End loop
         Scan_Token;
      end loop;
      return Tokens;
   end Scan_Tokens;

   procedure Scan_Token is
      C : Character;
   begin
      C := Advance;
      case C is
         when '(' =>
            Add_Token (TOK_LEFT_PAREN);
         when ')' =>
            Add_Token (TOK_RIGHT_PAREN);
         when '{' =>
            Add_Token (TOK_LEFT_BRACE);
         when '}' =>
            Add_Token (TOK_RIGHT_BRACE);
         when ',' =>
            Add_Token (TOK_COMMA);
         when '.' =>
            Add_Token (TOK_DOT);
         when '-' =>
            Add_Token (TOK_MINUS);
         when '+' =>
            Add_Token (TOK_PLUS);
         when ';' =>
            Add_Token (TOK_SEMICOLON);
         when '*' =>
            Add_Token (TOK_STAR);
         when '!' =>
            Add_Token (if Match ('=') then TOK_BANG_EQUAL else TOK_BANG);
         when '=' =>
            Add_Token (if Match ('=') then TOK_EQUAL_EQUAL else TOK_EQUAL);
         when '<' =>
            Add_Token (if Match ('=') then TOK_LESS_EQUAL else TOK_LESS);
         when '>' =>
            Add_Token (if Match ('=') then TOK_GREATER_EQUAL else TOK_GREATER);
         when '/' =>
            if Match ('/') then
               while Peek /= LF and then not Is_At_End loop
                  --  Skipping characters
                  Current := Current + 1;
               end loop;
            else
               Add_Token (TOK_SLASH);
            end if;
         when ' ' | CR | HT =>
            null;
         when LF =>
            if Line < Integer'Last then
               Line := Line + 1;
            end if;

         when others =>
            Put_Line ("Unknown character: " & Character'Image (C));

      end case;

   end Scan_Token;

   procedure Add_Token (TK : Token_Kind) is
   begin
      Add_Token (TK => TK, Lexeme => Source (Start .. Current - 1));
   end Add_Token;

   procedure Add_Token (TK : Token_Kind; Lexeme : String) is
      T : constant Token := Create_Token (TK => TK, S => Lexeme);
   begin
      Tokens.Append (T);
   end Add_Token;

end Scanners;
