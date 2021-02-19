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
  $white+                                  ;
  "--".*                                   ;
<0>  $digit+                               { lex (TokenInt . read)     }
<0>  config                                { lex' TokenConfig          }
<0>  run                                   { lex' TokenRun             }
<0>  iterations                            { lex' TokenIterations      }
<0>  model                                 { lex' TokenModel           }
<0>  const                                 { lex' TokenConst           }
<0>  couple                                { lex' TokenCouple          }
<0>  timeStep                              { lex' TokenTimeStep        }
<0>  totalTime                             { lex' TokenTotalTime       }
<0>  input                                 { lex' TokenInput           }
<0>  import                                { lex' TokenImport          }
<0>  output                                { lex' TokenOutput          }
<0>  return                                { lex' TokenReturn          }
<0>  technique                             { lex' TokenTechnique       }
<0>  FEM                                   { lex' TokenFEM             }
<0>  FVM                                   { lex' TokenFVM             }
<0>  boundaryField                         { lex' TokenBoundaryField   }
<0>  boundary                              { lex' TokenBoundary        }
<0>  Neumann                               { lex' TokenNeumann         }
<0>  Dirichlet                             { lex' TokenDirichlet       }
<0>  physics                               { lex' TokenPhysics         }
<0>  HeatTransfer                          { lex' TokenHeatTransfer    }
<0>  HeatConduction                        { lex' TokenHeatConduction  }
<0>  FluidFlow                             { lex' TokenFluidFlow       }
<0>  var                                   { lex' TokenV               }
<0>  solve                                 { lex' TokenSolve            }
<0>  PCG                                   { lex' TokenPCG             }
<0>  DIC                                   { lex' TokenDIC             }
<0>  solver                                { lex' TokenTsolver         }
<0>  preconditioner                        { lex' TokenTpreconditioner }
<0>  tolerance                             { lex' TokenTtolerance      }
<0>  relTol                                { lex' TokenTrelTol         }
<0>  Solvers                               { lex' TokenSolvers         }
<0>  Euler                                 { lex' TokenEuler           }
<0>  Gauss                                 { lex' TokenGauss           }
<0>  Linear                                { lex' TokenLinear          }
<0>  Orthogonal                            { lex' TokenOrthogonal      }
<0>  ddt                                   { lex' TokenNddt            }
<0>  grad                                  { lex' TokenNgrad           }
<0>  laplacian                             { lex' TokenNlaplacian      }
<0>  interpolation                         { lex' TokenNinterpolation  }
<0>  snGrad                                { lex' TokenNsnGrad         }
<0>  NumericalScheme                       { lex' TokenNumericalScheme }
<0>  SolvingTechnique                      { lex' TokenSolvingTechnique }
<0>  Backend                               { lex' TokenBackend         }
<0>  OpenFoam                              { lex' TokenOpenFoam        }
<0>  with                                  { lex' TokenWith            }
<0>  $alpha [$alpha $digit \_ \']*         { lex  TokenVar             }
<0>  \^                                    { lex' TokenPow             }
<0>  \:                                    { lex' TokenColon           }
<0>  \;                                    { lex' TokenSemi            }
<0>  \=                                    { lex' TokenEq              }
<0>  \∇\×                                  { lex' TokenNablaCross      }
<0>  \∇\•                                  { lex' TokenNablaDot        }
<0>  \∇\⊗                                  { lex' TokenNablaOuter      }
<0>  \∇                                    { lex' TokenNabla           }
<0>  \△                                    { lex' TokenTriangle        }
<0>  \∂                                    { lex' TokenPartial         }
<0>  \+                                    { lex' TokenPlus            }
<0>  \-                                    { lex' TokenMinus           }
<0>  \*                                    { lex' TokenTimes           }
<0>  \/                                    { lex' TokenDiv             }
<0>  \×                                    { lex' TokenCrossProduct    }
<0>  \•                                    { lex' TokenInnerProduct    }
<0>  \⊗                                    { lex' TokenOuterProduct    }
<0>  \⊗                                    { lex' TokenOuterProduct    }
<0>  \.                                    { lex' TokenDot             }
<0>  \,                                    { lex' TokenComma           }
<0>  \(                                    { lex' TokenLParen          }
<0>  \)                                    { lex' TokenRParen          }
<0>  \[                                    { lex' TokenLBracket        }
<0>  \]                                    { lex' TokenRBracket        }
<0>  \{                                    { lex' TokenLCurl           }
<0>  \}                                    { lex' TokenRCurl           }

<0>      "\begin{equations}"               { lex' TokenLatexBegin `andBegin` latex }
<latex>  "\end{equations}"                 { lex' TokenLatexEnd `andBegin` 0  }

<latex>  [0-9]+                             { lex (TokenLatex . TLInt . read) }
<latex>  \\ \\                              { lex (TokenLatex . TLSymbol)}
<latex>  "\times"|"\otimes"|"\div"|
         "\frac"|"\partial"|"\vec"|"\curl"|
         "\grad"|"\sqrt"|"\dot"|
         "\laplacian"                       { lex (TokenLatex . TLSymbol) }
<latex>  [\{ \} \_ \^ \+ \- \= \( \)]       { lex (TokenLatex . TLSymbol) }
<latex>  \\? [A-Za-z]+                      { lex (TokenLatex . TLIdent) }


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
  where
    loop =
      do  t@(Token _ tok) <- alexMonadScan'
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
