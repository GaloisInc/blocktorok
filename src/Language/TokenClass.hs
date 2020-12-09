{-|
Module      : Language.TokenClass
Description : LINK tokens
Copyright   : (c) Galois, Inc. 2020
License     : N/A
Maintainer  : chiw@galois.com
Stability   : experimental
Portability : N/A

This module defines the type of LINK tokens to be used by Alex for lexical
analysis.
-}

module Language.TokenClass
  ( TokenClass(..)
  , unLex
  ) where

-- | The type of LINK tokens
data TokenClass = TokenInt Int
                | TokenVar String
                | TokenConfig
                | TokenIterations
                | TokenModel
                | TokenConst
                | TokenCouple
                | TokenStep
                | TokenTotalTime
                | TokenInput
                | TokenOutput
                | TokenTechnique
                | TokenFEM
                | TokenFVM
                | TokenBoundary
                | TokenNeumann
                | TokenDirichlet
                | TokenPhysics
                | TokenHeatStructure
                | TokenFluidFlow
                | TokenV
                | TokenSolve
                | TokenColon
                | TokenSemi
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
                | TokenDot
                | TokenComma
                | TokenLParen
                | TokenRParen
                | TokenLBracket
                | TokenRBracket
                | TokenLCurl
                | TokenRCurl
                | TokenEOF
                deriving (Show)

-- | Given a @TokenClass@, return a string for use in error messages.
unLex :: TokenClass -> String
unLex (TokenInt i) = show i
unLex (TokenVar s) = show s
unLex TokenConfig = "config"
unLex TokenIterations = "iterations"
unLex TokenModel = "model"
unLex TokenConst = "const"
unLex TokenCouple = "couple"
unLex TokenStep = "step"
unLex TokenTotalTime = "totalTime"
unLex TokenTechnique = "technique"
unLex TokenInput = "input"
unLex TokenOutput = "output"
unLex TokenFEM = "FEM"
unLex TokenFVM = "FVM"
unLex TokenBoundary = "boundary"
unLex TokenNeumann = "Neumann"
unLex TokenDirichlet = "Dirichlet"
unLex TokenPhysics = "physics"
unLex TokenHeatStructure = "HeatStructure"
unLex TokenFluidFlow = "FluidFlow"
unLex TokenV = "var"
unLex TokenSolve = "solve"
unLex TokenColon = ":"
unLex TokenSemi = ";"
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
unLex TokenDot = "."
unLex TokenComma = ","
unLex TokenLParen = "("
unLex TokenRParen = ")"
unLex TokenLBracket = "["
unLex TokenRBracket = "]"
unLex TokenLCurl= "{"
unLex TokenRCurl = "}"
unLex TokenEOF = "<EOF>"
