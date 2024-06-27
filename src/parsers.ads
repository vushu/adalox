with Tokens; use Tokens;
with AST;
--  with Ada.Containers.Vectors;
package Parsers is
   Parser_Error  : exception;
   Runtime_Error : exception;
   type Variadic_Token_Kinds is array (Positive range <>) of Token_Kind;
   type Parser is tagged record
      Current : Natural := 0;
      Tokens  : Token_Vector;
   end record;
   function Parse (Self : in out Parser) return AST.Stmt_Vector;

private
   function Expression (Self : in out Parser) return AST.Expr_Access;
   function Match
     (Self : in out Parser; Types : Variadic_Token_Kinds) return Boolean;
   procedure Skip (Self : in out Parser);
   function Check (Self : Parser; T : Token_Kind) return Boolean;
   function Peek (Self : Parser) return Token;
   function Is_At_End (Self : Parser) return Boolean;
   function Consume
     (Self : in out Parser; T : Token_Kind; M : String) return Token;
   procedure Consume_Skip (Self : in out Parser; T : Token_Kind; M : String);
   function Advance (Self : in out Parser) return Token;
   function Previous (Self : Parser) return Token;

   procedure Synchronize (Self : in out Parser);
   function Var_Declaration(Self : in out Parser) return AST.Stmt_Access;
   function Declaration (Self : in out Parser) return AST.Stmt_Access;
   function Statement (Self : in out Parser) return AST.Stmt_Access;

end Parsers;
