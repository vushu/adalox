with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Literals;       use Literals;
with Lexeme_Strings; use Lexeme_Strings;
package body Scanners is
   function Scan_Tokens (Src : String) return Token_Vector is
      ------- STATE
      Start       : Natural              := 0;
      Source      : String (1 .. 10_240) := (others => ' ');
      Tokens      : Token_Vector;
      Current     : Natural              := 1;
      Line        : Natural;
      Source_Size : Positive;

      LF  : constant Character := Ada.Characters.Latin_1.LF;
      NUL : constant Character := Ada.Characters.Latin_1.NUL;
      CR  : constant Character := Ada.Characters.Latin_1.CR;
      HT  : constant Character := Ada.Characters.Latin_1.HT;

      procedure Add_Token (TK : Token_Kind; L : Literal) is
         T      : Token;
         Lexeme : constant String := Source (Start .. Current - 1);
      begin
         T := Create_Token (TK, Lexeme, L);
         Tokens.Append (T);
      end Add_Token;

      procedure Add_Token (TK : Token_Kind) is
      begin
         Add_Token (TK => TK, L => (Kind => Nothing));
      end Add_Token;

      function Is_At_End return Boolean is
      begin
         return Current > Source_Size;
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

      function Peek_Next return Character is
      begin
         if Current + 1 >= Source'Length then
            return NUL;
         end if;
         return Source (Current + 1);
      end Peek_Next;

      function Advance return Character is
         C : Character;
      begin
         --  Put_Line ("CURRENT: " & Integer'Image (Current));
         C       := Source (Current);
         Current := Current + 1;
         return C;
      end Advance;

      procedure Skip is
      begin
         Current := Current + 1;
      end Skip;

      procedure Handle_String is
         Lexeme     : Lexeme_String;
         String_Var : Literal (Kind => String_Type);
      begin
         while Peek /= '"' and then not Is_At_End loop
            if Peek = LF then
               Line := Line + 1;
            end if;
            Skip;
         end loop;
         if Is_At_End then
            Put_Line ("Unterminated string");
            return;
         end if;

         --  The closing ".
         Skip;

         Lexeme := Make_Lexeme_String (Source (Start + 1 .. Current - 1));
         String_Var.String_Val := Lexeme;

         --  String_Var := (String_Val => Lexeme);

         Add_Token (String_Token, String_Var);

      end Handle_String;

      function Is_Digit (C : Character) return Boolean is
      begin
         return C in '0' .. '9';
      end Is_Digit;

      function Is_Alpha (C : Character) return Boolean is
      begin
         return C in 'a' .. 'z' or else C in 'A' .. 'Z' or else C = '_';
      end Is_Alpha;

      function Is_Alpha_Numeric (C : Character) return Boolean is
      begin
         return Is_Alpha (C) or else Is_Digit (C);
      end Is_Alpha_Numeric;

      procedure Handle_Identifier is
      begin
         Put_Line ("Item : String");
      end Handle_Identifier;


      procedure Handle_Number is
         Digit : Float;
      begin
         while Is_Digit (Peek) loop
            Skip;
         end loop;
         if Peek = '.' and then Is_Digit (Peek_Next) then
            Skip;
            while Is_Digit (Peek) loop
               Skip;
            end loop;
         end if;
         Add_Token
           (Number_Token,
            Literal'(Float_Type, Float'Value (Source (Current .. Start))));
      end Handle_Number;

      procedure Scan_Token is
         C : Character;
      begin
         C := Advance;
         case C is
            when '(' =>
               Add_Token (Left_Paren_Token);
            when ')' =>
               Add_Token (Right_Paren_Token);
            when '{' =>
               Add_Token (Left_Brace_Token);
            when '}' =>
               Add_Token (Right_Brace_Token);
            when ',' =>
               Add_Token (Comma_Token);
            when '.' =>
               Add_Token (Dot_Token);
            when '-' =>
               Add_Token (Minus_Token);
            when '+' =>
               Add_Token (Plus_Token);
            when ';' =>
               Add_Token (Semicolon_Token);
            when '*' =>
               Add_Token (Star_Token);
            when '!' =>
               Add_Token
                 (if Match ('=') then Bang_Equal_Token else Bang_Token);
            when '=' =>
               Add_Token
                 (if Match ('=') then Equal_Equal_Token else Equal_Token);
            when '<' =>
               Add_Token
                 (if Match ('=') then Less_Equal_Token else Less_Token);
            when '>' =>
               Add_Token
                 (if Match ('=') then Greater_Equal_Token else Greater_Token);
            when '/' =>
               if Match ('/') then
                  while Peek /= LF and then not Is_At_End loop
                     --  Skipping characters
                     Skip;
                  end loop;
               else
                  Add_Token (Slash_Token);
               end if;
            when ' ' | CR | HT =>
               null;
            when LF =>
               if Line < Integer'Last then
                  Line := Line + 1;
               end if;
            when '"' =>
               Handle_String;
            when others =>
               if Is_Digit (C) then
                  Handle_Number;
               elsif Is_Alpha_Numeric (C) then
                  Put_Line ("asdfasf");
                  


               --  elsif Is_Alpha_Numeric (C) then

               end if;

               Put_Line ("Unknown character: " & Character'Image (C));

         end case;

      end Scan_Token;

   begin
      Source_Size                    := Src'Last;
      Source (Src'First .. Src'Last) := Src;
      while not Is_At_End loop
         Start := Current;
         Scan_Token;
      end loop;

      Tokens.Append (Create_Token (End_Of_File_Token, "", (Kind => Nothing)));
      return Tokens;
   end Scan_Tokens;

end Scanners;
