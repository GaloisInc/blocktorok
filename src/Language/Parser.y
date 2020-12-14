{
{-# OPTIONS -w #-}
module Language.Parser
  ( parseDecl
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Identifier
import Language.AST
import Solver.Technique
import Math
import Language.Lexer
import Language.TokenClass
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
      iterations      { Token _ TokenIterations }
      model           { Token _ TokenModel }
      const           { Token _ TokenConst }
      couple          { Token _ TokenCouple }
      step            { Token _ TokenStep }
      totalTime       { Token _ TokenTotalTime }
      input           { Token _ TokenInput }
      output          { Token _ TokenOutput }
      technique       { Token _ TokenTechnique }
      FEM             { Token _ TokenFEM }
      FVM             { Token _ TokenFVM }
      boundary        { Token _ TokenBoundary }
      Neumann         { Token _ TokenNeumann }
      Dirichlet       { Token _ TokenDirichlet }
      physics         { Token _ TokenPhysics }
      HeatTransfer   { Token _ TokenHeatTransfer }
      FluidFlow           { Token _ TokenFluidFlow }
      solve           { Token _ TokenSolve }
      var             { Token _ TokenV }
      varstr          { Token _ (TokenVar $$) }
      ':'             { Token _ TokenColon }
      ';'             { Token _ TokenSemi }
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
Prog :: { Prog }
Prog : Config ModelL CouplingL                                 { Prog $1 $2 $3 }

-- TODO: It would be nice if the order of the config fields didn't matter; I'll
-- look in to making that happen.
Config :: { Config }
Config : config '{' StepConfig DurationConfig '}'      { Config $3 $4 }

StepConfig :: { Int }
StepConfig : step ':' int ';'                                  { $3 }

DurationConfig :: { Duration }
DurationConfig : iterations ':' int ';'                        { Iterations $3 }
               | totalTime ':' int ';'                         { TotalTime $3 }

ModelL :: { Map Identifier Model }
ModelL : model Identifier ModelBody                            { Map.singleton $2 $3 }
       | ModelL model Identifier ModelBody                     { Map.insert $3 $4 $1 }

ModelBody :: { Model }
ModelBody : '{' InputDecl OutputDecl  SettingTechnique BoundaryDecl physicsType ConstDecls LibDecls  VarDecls EqL '}'   { mkModel $2 $3 $4 $5 $6 $7 $8 $9 $ reverse $10 }

ConstDecls :: { Map Identifier Int }
ConstDecls :                                                   { Map.empty }
           | ConstDecls const Identifier '=' int ';'           { Map.insert $3 $5 $1 }

LibDecls :: { Map Identifier (Identifier, Identifier) }
LibDecls :                                                     { Map.empty }
           | LibDecls  Identifier '=' ImportVar ';'            { Map.insert $2 $4 $1 }

ImportVar  :: {(Identifier, Identifier)}
ImportVar :  Identifier '.' Identifier                        { ($1,$3) }

VarDecls :: { Set Identifier }
VarDecls :                                                     { Set.empty }
         | VarDecls var Identifier ';'                         { Set.insert $3 $1 }


CouplingL :: { [Coupling] }
CouplingL :                                                    { [] }
          | CouplingL Coupling                                 { $2 : $1 }

Coupling :: { Coupling }
Coupling : couple Identifier Identifier '{'InputDecl OutputDecl  VarDecls EqL '}'                { Coupling $2 $3 $5 $6 $7 $ reverse $8}

Identifier :: { Identifier }
Identifier : varstr                                            { Identifier $1 }


InputDecl :: {Identifier}
InputDecl
    : input ':' varstr  ';'                                { Identifier $3 }

OutputDecl :: {Identifier}
OutputDecl
    : output ':' varstr  ';'                                { Identifier $3 }

-- solving
SettingTechnique :: { Technique }
SettingTechnique
     : technique ':' FEM ';'                                   { FEM }
     | technique ':' FVM ';'                                   { FVM }

-- boundary
BoundaryDecl :: { Boundary  }
BoundaryDecl
     : boundary ':'  Dirichlet '(' Identifier ')' ';'                            { Dirichlet $5 }
     | boundary ':'  Neumann '(' Identifier ')' ';'                              { Neumann $5 }

-- parameters for this type of physics
physicsType :: { PhysicsType }
physicsType
    : physics ':'  physicsTypeRHS  ';'                        { $3 }

-- parameters for this type of physics
physicsTypeRHS
    :  HeatTransfer '{' int '}'                             { HeatTransfer $3 }
    | FluidFlow '{' int '}'                                  { FluidFlow $3 }


-- mathematical expressions
Exp :: { Exp }
Exp  : '∇×' Exp                { NablaCross $2 }
      | '∇•' Exp               { NablaDot $2 }
      | '∇⊗' Exp               { NablaOuter $2 }
      | '∇' Exp                { NablaExp $2 }
      | '∇'                    { NablaSingle }
      | '△' Exp               { Laplacian $2 }
      | '-' Exp                { Negation $2 }
      | '(' Exp ')'            { Paran $2 }
      | Exp '+' Exp            { Plus $1 $3 }
      | Exp '-' Exp            { Minus $1 $3 }
      | Exp '*' Exp            { Times $1 $3 }
      | Exp '/' Exp            { Div $1 $3 }
      | Exp '×' Exp            { CrossProduct $1 $3 }
      | Exp '•' Exp            { InnerProduct $1 $3 }
      | Exp '⊗' Exp            { OuterProduct $1 $3 }
      | FnApp                  { $1}
      | Term                   { Term $1 }

FnApp : Identifier '('Identifier ')' {FnApp $1 $3}

Term :: { Term }
Term
      : int                     { Int $1 }
      | varstr                  { Var $1 }

EqL :: { [Equation] }
EqL :                           { [] }
    | EqL Eq                    { $2 : $1 }

Eq :: { Equation }
Eq : Exp '=' Exp ';'            { Equation $1 $3 }



{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseDecl :: FilePath -> String -> Either String Prog
parseDecl = runAlex' parse
}
