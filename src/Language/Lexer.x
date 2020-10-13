{
{-# OPTIONS -w  #-}
module Language.Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  $white+                               ;
  "--".*                                ;
  $digit+                               { lex (TokenInt . read)  }
  config                                { lex' TokenConfig       }
  model                                 { lex' TokenModel        }
  couple                                { lex' TokenCouple       }
  Solve                                 { lex' TokenSolve        }
  FEM                                   { lex' TokenFEM          }
  FVM                                   { lex' TokenFVM          }
  Space                                 { lex' TokenSpace        }
  $alpha [$alpha $digit \_ \']*         { lex  TokenVar          }
  \:                                    { lex' TokenColon        }
  \=                                    { lex' TokenEq           }
  \∇\×                                  { lex' TokenNablaCross   }
  \∇\•                                  { lex' TokenNablaDot     }
  \∇\⊗                                  { lex' TokenNablaOuter   }
  \∇                                    { lex' TokenNabla        }
  \△                                    { lex' TokenTriangle     }
  \+                                    { lex' TokenPlus         }
  \-                                    { lex' TokenMinus        }
  \*                                    { lex' TokenTimes        }
  \/                                    { lex' TokenDiv          }
  \×                                    { lex' TokenCrossProduct }
  \•                                    { lex' TokenInnerProduct }
  \⊗                                    { lex' TokenOuterProduct }
  \⊗                                    { lex' TokenOuterProduct }
  \Ω                                    { lex' TokenOmega        }
  \.                                    { lex' TokenDot          }
  \,                                    { lex' TokenComma        }
  \(                                    { lex' TokenLParen       }
  \)                                    { lex' TokenRParen       }
  \[                                    { lex' TokenLBracket     }
  \]                                    { lex' TokenRBracket     }
  \{                                    { lex' TokenLCurl        }
  \}                                    { lex' TokenRCurl        }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TokenInt Int
  | TokenVar String
  | TokenConfig
  | TokenModel
  | TokenCouple
  | TokenSolve
  | TokenFEM
  | TokenFVM
  | TokenSpace
  | TokenColon
  | TokenEq
  | TokenTriangle
  | TokenNablaCross
  | TokenNablaDot
  | TokenNablaOuter
  | TokenNabla
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenCrossProduct
  | TokenInnerProduct
  | TokenOuterProduct
  | TokenOmega
  | TokenDot
  | TokenComma
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenLCurl
  | TokenRCurl
  | TokenEOF
  deriving ( Show )

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex (TokenInt i) = show i
unLex (TokenVar s) = show s
unLex TokenConfig = "config"
unLex TokenModel = "model"
unLex TokenCouple = "couple"
unLex TokenFEM = "FEM"
unLex TokenFVM = "FVM"
unLex TokenSpace = "Space"
unLex TokenColon = ":"
unLex TokenEq = "="
unLex TokenTriangle = "△"
unLex TokenNablaCross = "∇×"
unLex TokenNablaDot = "∇•"
unLex TokenNablaOuter = "∇⊗"
unLex TokenNabla = "∇"
unLex TokenPlus = "+"
unLex TokenMinus = "-"
unLex TokenTimes = "*"
unLex TokenDiv = "/"
unLex TokenCrossProduct ="×"
unLex TokenInnerProduct="•"
unLex TokenOuterProduct ="⊗"
unLex TokenOmega = "Ω"
unLex TokenDot = "."
unLex TokenComma = ","
unLex TokenLParen = "("
unLex TokenRParen = ")"
unLex TokenLBracket = "["
unLex TokenRBracket = "]"
unLex TokenLCurl= "{"
unLex TokenRCurl = "}"
unLex TokenEOF = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
