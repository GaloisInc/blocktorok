{
{-# OPTIONS -w #-}
module Parser( parseDecl) where


import Language
import Lexer

}

-- The expression language used here comes straight from the happy
-- documentation with virtually no changes (open, so TokenOB/TokenCB were
-- changed to TokenLParen/TokenRParen

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%token
      int             { Token _ (TokenInt $$) }
      var             { Token _ (TokenVar $$) }
      '='             { Token _ TokenEq }
      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '•'             { Token _ TokenInnerProduct }
      '△'             { Token _ TokenTriangle }
      '∇'             { Token _ TokenNabla }
      '/'             { Token _ TokenDiv }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

%%
Decl : DeclL                                  {D_Stmts (reverse $1)}

DeclL :                                       { [] }
    | DeclL Stmt                              { $2 : $1 }

Stmt  : Exp '=' Exp            { Equation $1 $3}


Exp  : '∇' '•' Exp             { Divergence $3 }
      |'∇' Exp                 { Nabla $2 }
      | '△' Exp                { Laplacian $2 }
      | '-' Exp                { Negation $2 }
      | '(' Exp ')'            { Paran $2 }
      | Exp '+' Exp            { Plus $1 $3 }
      | Exp '-' Exp            { Minus $1 $3 }
      | Exp '*' Exp            { Times $1 $3 }
      | Exp '•' Exp            { InnerProduct $1 $3 }
      | Exp '/' Exp            { Div $1 $3 }
      | Term                   { Term $1 }

Term
      : int                     { Int $1 }
      | var                     { Var $1 }


{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseDecl :: FilePath -> String -> Either String Decl
parseDecl = runAlex' parse
}
