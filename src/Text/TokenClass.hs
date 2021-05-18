{-|
Module      : Text.TokenClass
Description : LINK tokens
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com
Stability   : experimental
Portability : N/A

This module defines the type of LINK tokens to be used by Alex for lexical
analysis.
-}

module Text.TokenClass
  ( TokenClass(..)
  , TokenLatex(..)
  , unLex
  ) where

-- | The type of LINK tokens
data TokenClass =
    TokenFloat Double
  | TokenInt Integer
  | TokenVar String
  | TokenConfig
  | TokenRun
  | TokenTimeDomain
  | TokenTransient
  | TokenSteady
  | TokenIterationsTime
  | TokenIterationsCoupling
  | TokenIterationsInner
  | TokenModel
  | TokenConst
  | TokenCouple
  | TokenCoupledSurfaces
  | TokenTimeStep
  | TokenTotalTime
  | TokenInput
  | TokenImport
  | TokenOutput
  | TokenReturn
  | TokenTechnique
  | TokenFEM
  | TokenFVM
  | TokenBoundary
  | TokenBoundaryField
  | TokenNeumann
  | TokenDirichlet
  | TokenPhysics
  | TokenHeatTransfer
  | TokenHeatConduction
  | TokenFluidFlow
  | TokenV
  | TokenSolve
  | TokenPCG
  | TokenDIC
  | TokenTsolver
  | TokenTpreconditioner
  | TokenTtolerance
  | TokenTrelTol
  | TokenSolvers
  | TokenEuler
  | TokenGauss
  | TokenLinear
  | TokenOrthogonal
  | TokenNddt
  | TokenNgrad
  | TokenNlaplacian
  | TokenNinterpolation
  | TokenNsnGrad
  | TokenNumericalScheme
  | TokenSolvingTechnique
  | TokenMesh
  | TokenBackend
  | TokenOpenFoam
  | TokenSu2
  | TokenFormat
  | TokenSharedParams
  | TokenPlotting
  | TokenPow
  | TokenColon
  | TokenSemi
  | TokenEq
  | TokenTriangle
  | TokenNablaCross
  | TokenNablaDot
  | TokenNablaOuter
  | TokenNabla
  | TokenPartial
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenCrossProduct
  | TokenInnerProduct
  | TokenOuterProduct
  | TokenDot
  | TokenWith
  | TokenComma
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenLCurl
  | TokenRCurl
  | TokenEOF
  | TokenLatexBegin
  | TokenLatexEnd
  | TokenLatex TokenLatex
  deriving (Eq, Show)

data TokenLatex =
    TLIdent String
  | TLInt Integer
  | TLSymbol String
  deriving (Eq, Show)

-- | Given a @TokenClass@, return a string for use in error messages.
unLex :: TokenClass -> String
unLex (TokenFloat d) = show d
unLex (TokenInt i) = show i
unLex (TokenVar s) = show s
unLex TokenConfig = "config"
unLex TokenRun ="Run"
unLex TokenTimeDomain ="time domain"
unLex TokenTransient = "transient"
unLex TokenSteady = "steady"
unLex TokenIterationsTime = "time iterations"
unLex TokenIterationsCoupling= "coupling Iterations"
unLex TokenIterationsInner = "inner iterations"
unLex TokenModel = "model"
unLex TokenConst = "const"
unLex TokenCouple = "couple"
unLex TokenCoupledSurfaces = "coupled surfaces"
unLex TokenTimeStep  = "TimeStep"
unLex TokenTotalTime = "totalTime"
unLex TokenTechnique = "technique"
unLex TokenInput = "input"
unLex TokenImport = "import"
unLex TokenOutput = "output"
unLex TokenReturn = "return"
unLex TokenFEM = "FEM"
unLex TokenFVM = "FVM"
unLex TokenBoundary = "boundary"
unLex TokenBoundaryField = "BoundaryField"
unLex TokenNeumann = "Neumann"
unLex TokenDirichlet = "Dirichlet"
unLex TokenPhysics = "physics"
unLex TokenHeatTransfer = "HeatTransfer"
unLex TokenHeatConduction  = "HeatConduction"
unLex TokenFluidFlow = "FluidFlow"
unLex TokenV = "var"
unLex TokenSolve = "solve"
unLex TokenPow = "^"
unLex TokenColon = ":"
unLex TokenSemi = ";"
unLex TokenEq = "="
unLex TokenTriangle = "△"
unLex TokenNablaCross = "∇×"
unLex TokenNablaDot = "∇•"
unLex TokenNablaOuter = "∇⊗"
unLex TokenNabla = "∇"
unLex TokenPartial = "∂"
unLex TokenPlus = "+"
unLex TokenMinus = "-"
unLex TokenTimes = "*"
unLex TokenDiv = "/"
unLex TokenCrossProduct ="×"
unLex TokenInnerProduct="•"
unLex TokenOuterProduct ="⊗"
unLex TokenDot = "."
unLex TokenPCG = "PCG"
unLex TokenDIC = "DIC"
unLex TokenTsolver ="solver"
unLex TokenTpreconditioner = "preconditioner"
unLex TokenTtolerance = "tolerance"
unLex TokenTrelTol = "relTol"
unLex TokenSolvers = "solvers"
unLex TokenEuler = "Euler"
unLex TokenGauss = "Gauss"
unLex TokenLinear = "Linear"
unLex TokenOrthogonal = "Orthogonal"
unLex TokenNddt = "ddt"
unLex TokenNgrad = "grad"
unLex TokenNlaplacian = "laplacian"
unLex TokenNinterpolation = "interpolation"
unLex TokenNsnGrad ="snGrad"
unLex TokenNumericalScheme = "NumericalScheme"
unLex TokenSolvingTechnique = "SolvingTechnique"
unLex TokenMesh  = "Mesh"
unLex TokenBackend = "backend"
unLex TokenOpenFoam = "OpenFoam"
unLex TokenSu2 = "Su2"
unLex TokenFormat = "format"
unLex TokenSharedParams ="shared params"
unLex TokenPlotting = "plotting"
unLex TokenWith = "with"
unLex TokenComma = ","
unLex TokenLParen = "("
unLex TokenRParen = ")"
unLex TokenLBracket = "["
unLex TokenRBracket = "]"
unLex TokenLCurl= "{"
unLex TokenRCurl = "}"
unLex TokenLatexBegin = "\\begin{equations}"
unLex TokenLatexEnd = "\\end{equations}"
unLex (TokenLatex (TLInt i)) = show i
unLex (TokenLatex (TLIdent s)) = s
unLex (TokenLatex (TLSymbol s)) = s
unLex TokenEOF = "<EOF>"
