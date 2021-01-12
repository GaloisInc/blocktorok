{-|
Module      : Language.Parser
Description : Parser for the LINK language
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Language.Parser
  ( parseDecl
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.AST
import Language.Identifier
import Language.Lexer
import Language.Token
import Language.TokenClass

import Math

import Physics.Model

import Solver.Technique

import Text.Parsec
import Text.Parsec.Expr

parseText :: Parser a -> String -> Either ParseError a
parseText p = parseNamedText p "<string>"

parseNamedText :: Parser a -> String -> String -> Either ParseError a
parseNamedText p n s =
  case llex s of
    Left e -> parse (reportLexError e) n []
    Right xs -> parse p n xs

reportLexError :: String -> Parser a
reportLexError msg = fail ("lexical error: " ++ msg)

parseDecl :: FilePath -> String -> Either ParseError Prog
parseDecl = parseNamedText parseProg

parseProg :: Parser Prog
parseProg =
  do cfg <- parseConfig
     models <- parseModels
     Prog cfg models <$> parseCouplings

parseConfig :: Parser Config
parseConfig =
  do tok' TokenConfig
     tok' TokenLCurl
     steps <- parseStepConfig
     duration <- parseDurationConfig
     tok' TokenRCurl
     return $ Config steps duration
  where
    parseStepConfig =
      do tok' TokenStep
         tok' TokenColon
         n <- number
         tok' TokenSemi
         return n

    parseDurationConfig =
      do mode <- tok TokenIterations <|> tok TokenTotalTime
         tok' TokenColon
         n <- number
         tok' TokenSemi
         return $ case mode of
                    TokenIterations -> Iterations n
                    TokenTotalTime -> TotalTime n

parseModels :: Parser (Map Identifier Model)
parseModels =
  do models <- many1 parseModel
     return $ Map.fromList models
  where
    parseModel =
      do tok' TokenModel
         i <- parseIdentifier
         model <- parseModelBody
         return (i, model)

    parseModelBody =
      do tok' TokenLCurl
         inputDecl <- parseInputDecl
         outputDecl <- parseOutputDecl
         technique <- parseSettingTechnique
         boundaryDecl <- parseBoundaryDecl
         physType <- parsePhysicsType
         consts <- parseConstDecls
         libs <- parseLibDecls
         vs <- parseVarDecls
         eqs <- parseEqs
         tok' TokenRCurl
         return $ mkModel inputDecl outputDecl technique boundaryDecl physType consts libs vs eqs

parseIdentifier :: Parser Identifier
parseIdentifier =
  do Identifier <$> variable

parseInputDecl :: Parser Identifier
parseInputDecl =
  do tok' TokenInput
     tok' TokenColon
     var <- variable
     tok' TokenSemi
     return $ Identifier var

parseOutputDecl :: Parser Identifier
parseOutputDecl =
  do tok' TokenOutput
     tok' TokenColon
     var <- variable
     tok' TokenSemi
     return $ Identifier var

parseSettingTechnique :: Parser Technique
parseSettingTechnique =
  do tok' TokenTechnique
     tok' TokenColon
     technique <- tok TokenFEM <|> tok TokenFVM
     tok' TokenSemi
     return $ case technique of
                TokenFEM -> FEM
                TokenFVM -> FVM

parseBoundaryDecl :: Parser Boundary
parseBoundaryDecl =
  do tok' TokenBoundary
     tok' TokenColon
     methodTok <- tok TokenDirichlet <|> tok TokenNeumann
     tok' TokenLParen
     i <- parseIdentifier
     tok' TokenRParen
     tok' TokenSemi
     return $ case methodTok of
                TokenDirichlet -> Dirichlet i
                TokenNeumann -> Neumann i

parsePhysicsType :: Parser PhysicsType
parsePhysicsType =
  do tok' TokenPhysics
     tok' TokenColon
     rhs <- parsePhysicsTypeRHS
     tok' TokenSemi
     return rhs
  where
    parsePhysicsTypeRHS =
      do t <- tok TokenHeatTransfer <|> tok TokenFluidFlow
         tok' TokenLCurl
         n <- number
         tok' TokenRCurl
         return $ case t of
                    TokenHeatTransfer -> HeatTransfer n
                    TokenFluidFlow -> FluidFlow n

parseConstDecls :: Parser (Map Identifier Int)
parseConstDecls =
  do decls <- many parseConstDecl
     return $ Map.fromList decls
  where
    parseConstDecl =
      do tok' TokenConst
         i <- parseIdentifier
         tok' TokenEq
         n <- number
         tok' TokenSemi
         return (i, n)

parseLibDecls :: Parser (Map Identifier (Identifier, Identifier))
parseLibDecls =
  do decls <- many parseLibDecl
     return $ Map.fromList decls
  where
    parseLibDecl =
      do i <- parseIdentifier
         tok' TokenEq
         lib <- parseImport
         tok' TokenSemi
         return (i, lib)

    parseImport =
      do scope <- parseIdentifier
         tok' TokenDot
         m <- parseIdentifier
         return (scope, m)

parseVarDecls :: Parser (Set Identifier)
parseVarDecls =
  do decls <- many parseVarDecl
     return $ Set.fromList decls
  where
    parseVarDecl =
      do tok' TokenV
         i <- parseIdentifier
         tok' TokenSemi
         return i

parseEqs :: Parser [Equation]
parseEqs = many parseEq
  where
    parseEq =
      do lhs <- parseExp
         tok' TokenEq
         rhs <- parseExp
         tok' TokenSemi
         return $ Equation lhs rhs

parseExp :: Parser Exp
parseExp = buildExpressionParser opTable parseTerm
  where
    opTable = [ [prefix TokenNablaCross NablaCross, prefix TokenNablaDot NablaDot, prefix TokenNablaOuter NablaOuter]
              , [prefix TokenTriangle Laplacian, prefix TokenNabla NablaExp]
              , [binary TokenTimes Times AssocLeft, binary TokenInnerProduct InnerProduct AssocLeft, binary TokenCrossProduct CrossProduct AssocLeft, binary TokenOuterProduct OuterProduct AssocLeft]
              , [binary TokenDiv Div AssocLeft]
              , [binary TokenPlus Plus AssocLeft, binary TokenMinus Minus AssocLeft]
              ]

    binary opTok fun = Infix (do { tok' opTok; return fun })
    prefix opTok fun = Prefix (do { tok' opTok; return fun })

    parseTerm = (IntE <$> number) <|> (Var <$> variable) <|> parseFnApp <|> parseParens
      where
        parseFnApp :: Parser Exp
        parseFnApp =
          do f <- parseIdentifier
             tok' TokenLParen
             arg <- parseIdentifier
             tok' TokenRParen
             return $ FnApp f arg

        parseParens :: Parser Exp
        parseParens =
          do tok' TokenLParen
             e <- parseExp
             tok' TokenRParen
             return $ Paran e

parseCouplings = undefined
