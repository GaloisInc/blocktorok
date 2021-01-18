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
     timeStep <- parseTimeStepConfig
     duration <- parseDurationConfig
     eqs <- parseEqs
     tok' TokenRCurl
     return $ Config timeStep duration eqs
  where
    parseTimeStepConfig =
      do tok' TokenTimeStep
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
                    _ -> error "This can't happen"

parseModels :: Parser (Map Identifier Model)
parseModels =
  do models <- many1 parseModel
     return $ Map.fromList models
  where
    parseModel =
      do tok' TokenModel
         i <- parseIdentifier
         tok' TokenLParen
         j <-  parseIdentifier
         tok' TokenRParen
         model <- parseModelBody j
         return (i, model)

    parseModelBody inputDecl =
      do tok' TokenLCurl
         --inputDecl <- parseInputDecl
         outputDecl <- parseOutputDecl
         technique <- parseSettingTechnique
         boundaryDecl <- parseBoundaryDecl
         physType <- parsePhysicsType
         consts <- parseConstDecls
         libs <- parseLibDecls
         vs <- parseVarDecls
         eqs <- parseEqs
         --outputDecl <- parseReturnDecl
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

parseReturnDecl :: Parser Identifier
parseReturnDecl =
   do tok' TokenReturn
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
                _ -> error "This can't happen"

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
                _ -> error "This can't happen"

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
                    _ -> error "This can't happen"

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
parseExp = parseExp1 `chainl1` pAddOp
  where
    pAddOp :: Parser (Exp -> Exp -> Exp)
    pAddOp =
      do op <- choice (tok <$> [TokenPlus, TokenMinus])
         return $ case op of
                    TokenPlus -> Plus
                    TokenMinus -> Minus
                    _ -> error "This can't happen"

    parseExp1 :: Parser Exp
    parseExp1 = parseExp2 `chainl1` return Div

    parseExp2 :: Parser Exp
    parseExp2 = parseExp3 `chainl1` pMulOp
      where
        pMulOp :: Parser (Exp -> Exp -> Exp)
        pMulOp =
          do op <- choice (tok <$> [TokenTimes, TokenInnerProduct, TokenCrossProduct, TokenOuterProduct])
             return $ case op of
                        TokenTimes -> Times
                        TokenInnerProduct -> InnerProduct
                        TokenCrossProduct -> CrossProduct
                        TokenOuterProduct -> OuterProduct
                        _ -> error "This can't happen"

    parseExp3 :: Parser Exp
    parseExp3 = try p <|> (tok' TokenNabla >> return NablaSingle) <|> parseExp4
      where
        p =
          do op <- choice [tok TokenTriangle, tok TokenNabla]
             let f = case op of
                       TokenTriangle -> Laplacian
                       TokenNabla -> NablaExp
                       _ -> error "This can't happen"
             f <$> parseExp3

    parseExp4 :: Parser Exp
    parseExp4 = p <|> parseExp5
      where
        p =
          do op <- choice [tok TokenNablaCross, tok TokenNablaDot, tok TokenNablaOuter]
             let f = case op of
                       TokenNablaCross -> NablaCross
                       TokenNablaDot -> NablaDot
                       TokenNablaOuter -> NablaOuter
                       _ -> error "This can't happen"
             f <$> parseExp4

    parseExp5 :: Parser Exp
    parseExp5 = (IntE <$> number)
             <|> try parseFnApp
             <|> (Var <$> variable)
             <|> parseNeg
             <|> parseParens
      where
        parseFnApp =
          do f <- parseIdentifier
             tok' TokenLParen
             arg <- parseIdentifier
             tok' TokenRParen
             return $ FnApp f arg

        parseNeg =
          do tok' TokenMinus
             Negation <$> parseExp5

        parseParens =
          do tok' TokenLParen
             e <- parseExp
             tok' TokenRParen
             return $ Paran e

parseCouplings :: Parser [Coupling]
parseCouplings = many parseCoupling
  where
    parseCoupling =
      do tok' TokenCouple
         ma <- parseIdentifier
         mb <- parseIdentifier
         tok' TokenLCurl
         i <- parseInputDecl
         o <- parseOutputDecl
         vs <- parseVarDecls
         eqs <- parseEqs
         tok' TokenRCurl
         return $ Coupling ma mb i o vs eqs
