{
{-# OPTIONS -w #-}
module Language.Parser
  ( parseDecl
  ) where


import Language.AST
import Solver.Technique
import Math
import Language.Lexer
import Physics.Type
import Physics.Model

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

--- higher means lower Precendence
%left '+' '-'
%left '/'
%left '*' '•'  '×' '⊗'
%right '△' '∇'
%right '∇×' '∇•' '∇⊗'
%left '='
%nonassoc '(' ')' '[' ']' '{' '}'

%token
      int             { Token _ (TokenInt $$) }
      config          { Token _ TokenConfig }
      model           { Token _ TokenModel }
      couple          { Token _ TokenCouple }
      Solve           { Token _ TokenSolve }
      FEM             { Token _ TokenFEM }
      FVM             { Token _ TokenFVM }
      Space           { Token _ TokenSpace }
      var             { Token _ (TokenVar $$) }
      ':'             { Token _ TokenColon }
      '='             { Token _ TokenEq }
      '∇×'            { Token _ TokenNablaCross }
      '∇•'            { Token _ TokenNablaDot }
      '∇⊗'            { Token _ TokenNablaOuter }
      '∇'             { Token _ TokenNabla }
      '△'             { Token _ TokenTriangle }
      '+'             { Token _ TokenPlus }
      '-'             { Token _ TokenMinus }
      '*'             { Token _ TokenTimes }
      '/'             { Token _ TokenDiv }
      '×'             { Token _ TokenCrossProduct }
      '•'             { Token _ TokenInnerProduct }
      '⊗'             { Token _ TokenOuterProduct }
      'Ω'             { Token _ TokenOmega }
      '.'             { Token _ TokenDot }
      ','             { Token _ TokenComma }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }
      '['            { Token _ TokenLBracket }
      ']'            { Token _ TokenRBracket }
      '{'            { Token _ TokenLCurl}
      '}'            { Token _ TokenRCurl }

%%
-- An entire program is a config block followed by one or more models and an
-- appropriate number of couplings (this appropriate number is not asserted by
-- the parser)
Prog : Config ModelL CouplingL                { Prog $1 (reverse $2) (reverse $3) }

Config : config ':' '{' '}'                   { Config 17 (Iterations 17) }

ModelL :                                      { [] }
       | ModelL Model                         { $2 : $1 }

Model : model Term PhysicsModel               { $3 }

CouplingL :                                   { [] }
          | CouplingL Coupling                { $2 : $1 }

Coupling : couple ':' '{' '}'                 { Coupling 17 }

-- Decl : DeclL                                  {DStmts (reverse $1)}

-- DeclL :                                       { [] }
--     | DeclL Stmt                              { $2 : $1 }

PhysicsModel : '{' SettingSolve ',' SettingSpace '}' { Physics.Model.mkModel LaminarFlow $2 $4}

-- Stmt  : Omega '.' Exp '=' Exp                  { Equation $3 $5 $1}
--       | model Term PhysicsModel                { Box $2 $3}

-- solving
SettingSolve
     : Solve '=' FEM                    { FEM }
     | Solve '=' FVM                    { FVM }

-- Omega
Omega
    : 'Ω' Term                 { Omega $2}

SettingSpace
    : Space '='  Omega          { $3 }

-- mathematical expressions
-- Exp  : '∇×' Exp                { NablaCross $2 }
--       | '∇•' Exp               { NablaDot $2 }
--       | '∇⊗' Exp               { NablaOuter $2 }
--       | '∇' Exp                { NablaExp $2 }
--       | '∇'                    { NablaSingle }
--       | '△' Exp               { Laplacian $2 }
--       | '-' Exp                { Negation $2 }
--       | '(' Exp ')'            { Paran $2 }
--       | Exp '+' Exp            { Plus $1 $3 }
--       | Exp '-' Exp            { Minus $1 $3 }
--       | Exp '*' Exp            { Times $1 $3 }
--       | Exp '/' Exp            { Div $1 $3 }
--       | Exp '×' Exp            { CrossProduct $1 $3 }
--       | Exp '•' Exp            { InnerProduct $1 $3 }
--       | Exp '⊗' Exp            { OuterProduct $1 $3 }
--       | Term                   { Term $1 }

Term
      : int                     { Int $1 }
      | var                     { Var $1 }



{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseDecl :: FilePath -> String -> Either String Prog
parseDecl = runAlex' parse
}
