{
{-# OPTIONS -w  #-}
module Text.Lexer
  ( Token(..)
  , AlexPosn(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  , llex
  ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )

import Text.TokenClass
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  $white+                               ;
  "--".*                                ;
  $digit+                               { lex (TokenInt . read)  }
  config                                { lex' TokenConfig       }
  iterations                            { lex' TokenIterations   }
  model                                 { lex' TokenModel        }
  const                                 { lex' TokenConst        }
  couple                                { lex' TokenCouple       }
  timeStep                              { lex' TokenTimeStep        }
  totalTime                             { lex' TokenTotalTime    }
  input                                 { lex' TokenInput        }
  output                                { lex' TokenOutput       }
  return                                { lex' TokenReturn       }
  technique                             { lex' TokenTechnique    }
  FEM                                   { lex' TokenFEM          }
  FVM                                   { lex' TokenFVM          }
  boundaryField                         { lex' TokenBoundaryField }
  boundary                              { lex' TokenBoundary     }
  Neumann                               { lex' TokenNeumann      }
  Dirichlet                             { lex' TokenDirichlet    }
  physics                               { lex' TokenPhysics      }
  HeatTransfer                          { lex' TokenHeatTransfer }
  HeatConduction                        { lex' TokenHeatConduction}
  FluidFlow                             { lex' TokenFluidFlow}
  var                                   { lex' TokenV            }
  solve                                 { lex' TokenSolve        }
  $alpha [$alpha $digit \_ \']*         { lex  TokenVar          }
  \^                                    { lex' TokenPow          }
  \:                                    { lex' TokenColon        }
  \;                                    { lex' TokenSemi         }
  \=                                    { lex' TokenEq           }
  \∇\×                                  { lex' TokenNablaCross   }
  \∇\•                                  { lex' TokenNablaDot     }
  \∇\⊗                                  { lex' TokenNablaOuter   }
  \∇                                    { lex' TokenNabla        }
  \△                                    { lex' TokenTriangle     }
  \∂                                    { lex' TokenPartial      }
  \+                                    { lex' TokenPlus         }
  \-                                    { lex' TokenMinus        }
  \*                                    { lex' TokenTimes        }
  \/                                    { lex' TokenDiv          }
  \×                                    { lex' TokenCrossProduct }
  \•                                    { lex' TokenInnerProduct }
  \⊗                                    { lex' TokenOuterProduct }
  \⊗                                    { lex' TokenOuterProduct }
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

-- LINK lexer
llex :: String -> Either String [Token]
llex str = runAlex str loop
  where loop = do t@(Token _ tok) <- alexMonadScan'
                  if tok == TokenEOF then
                    return [t]
                  else
                    do toks <- loop
                       return $ t:toks

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
