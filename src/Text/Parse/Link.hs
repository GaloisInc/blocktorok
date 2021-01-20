{-|
Module      : Text.Parse.Link
Description : Parser for the LINK language
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines the parser for the LINK language, using Parsec.
-}

module Text.Parse.Link
  ( parseDecl
  ) where

import Control.Monad.Reader

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Link.AST
import Data.Link.Identifier
import Text.Lexer
import qualified Text.Parse.Units as UP
import Text.Token
import Text.TokenClass
import Data.Math
import Data.Physics.Model
import Data.Solver.Technique
import qualified Data.Units.UnitExp as U
import Data.Units.SymbolTable
import Data.Units.SI
import Data.Units.SI.Prefixes

import Language.Haskell.TH.Syntax (Name)
import Text.Parsec

prefixStrs, unitStrs :: [String]
prefixStrs =
  [ "da"
  , "h"
  , "k"
  , "M"
  , "G"
  , "T"
  , "P"
  , "E"
  , "Z"
  , "Y"
  , "d"
  , "c"
  , "m"
  , "μ"
  , "n"
  , "p"
  , "f"
  , "a"
  , "z"
  , "y"
  ]

unitStrs =
  [ "m"
  , "g"
  , "s"
  , "min"
  , "h"
  , "A"
  , "K"
  , "mol"
  , "cd"
  , "Hz"
  , "L"
  , "N"
  , "Pa"
  , "J"
  , "W"
  , "C"
  , "V"
  , "F"
  , "Ω"
  , "S"
  , "Wb"
  , "T"
  , "H"
  , "lm"
  , "rad"
  , "lx"
  , "Bq"
  , "Gy"
  , "Sv"
  , "kat"
  , "deg"
  , "arcminute"
  , "arcsecond"
  , "hectare"
  , "t"
  ]

table :: SymbolTable Name Name
table = case mkSymbolTable (zip prefixStrs siPrefixes) (zip unitStrs siUnits) of
          Left e -> error e
          Right st -> st

parseNamedText :: Parser a -> String -> String -> Either ParseError a
parseNamedText p n s =
  case llex s of
    Left e -> flip runReader table $ runParserT (reportLexError e) () n []
    Right xs -> flip runReader table $ runParserT p () n xs

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
         boundaryDecl <- parseBoundaryDecl --parseBoundaryFieldsDecl
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

parseBoundaryType :: Parser BoundaryType
parseBoundaryType =
  do methodTok <- tok TokenDirichlet <|> tok TokenNeumann
     return $ case methodTok of
             TokenDirichlet -> Dirichlet
             TokenNeumann -> Neumann
             _ -> error "This can't happen"

parseBoundaryTypeDecl :: Parser Boundary
parseBoundaryTypeDecl =
 do tok' TokenBoundary
    tok' TokenColon
    method <- parseBoundaryType
    tok' TokenLParen
    id <- parseIdentifier
    tok' TokenRParen
    tok' TokenSemi
    return (T method id)


parseBoundaryField :: Parser BoundaryField
parseBoundaryField =
  do tok' TokenLParen
     id <- parseIdentifier
     tok' TokenComma
     method <- parseBoundaryType
     tok' TokenComma
     n <- number
     tok' TokenRParen
     tok' TokenSemi
     return (BoundaryField id method  n)

parseBoundaryFieldsDecl :: Parser Boundary
parseBoundaryFieldsDecl =
  do
    tok' TokenBoundaryField
    tok' TokenColon
    x <- (many parseBoundaryField)
    return (F x)

parseBoundaryDecl :: Parser Boundary
parseBoundaryDecl =
  do
    parseBoundaryFieldsDecl <|> parseBoundaryTypeDecl

parsePhysicsType :: Parser PhysicsType
parsePhysicsType =
  do tok' TokenPhysics
     tok' TokenColon
     rhs <- parsePhysicsTypeRHS
     tok' TokenSemi
     return rhs
  where
    parsePhysicsTypeRHS =
      do t <- tok TokenHeatTransfer <|> tok TokenFluidFlow <|> tok TokenHeatConduction
         tok' TokenLCurl
         n <- parseIdentifier
         tok' TokenRCurl
         return $ case t of
                    TokenHeatTransfer -> HeatTransfer n
                    TokenFluidFlow -> FluidFlow n
                    TokenHeatConduction -> HeatConduction n
                    _ -> error "This can't happen"

parseConstDecls :: Parser (Map Identifier (Integer, U.UnitExp Name Name))
parseConstDecls =
  do decls <- many parseConstDecl
     return $ Map.fromList decls
  where
    parseConstDecl =
      do tok' TokenConst
         i <- parseIdentifier
         tok' TokenEq
         n <- number
         u <- UP.parseUnit
         tok' TokenSemi
         return (i, (n, u))

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

parseVarDecls :: Parser (Map Identifier (U.UnitExp Name Name))
parseVarDecls =
  do decls <- many parseVarDecl
     return $ Map.fromList decls
  where
    parseVarDecl =
      do tok' TokenV
         i <- parseIdentifier
         tok' TokenColon
         u <- UP.parseUnit
         tok' TokenSemi
         return (i, u)

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
    parseExp3 = parseExp4 `chainl1` pPow
      where
        pPow :: Parser (Exp -> Exp -> Exp)
        pPow = tok' TokenPow >> return Pow

    parseExp4 :: Parser Exp
    parseExp4 = try p <|> (tok' TokenNabla >> return NablaSingle) <|> parseExp5
      where
        p =
          do op <- choice [tok TokenTriangle, tok TokenNabla, tok TokenPartial]
             let f = case op of
                       TokenTriangle -> Laplacian
                       TokenNabla -> NablaExp
                       TokenPartial -> Partial
                       _ -> error "This can't happen-4"
             f <$> parseExp3

    parseExp5 :: Parser Exp
    parseExp5 = p <|> parseExp6
      where
        p =
          do op <- choice [tok TokenNablaCross, tok TokenNablaDot, tok TokenNablaOuter]
             let f = case op of
                       TokenNablaCross -> NablaCross
                       TokenNablaDot -> NablaDot
                       TokenNablaOuter -> NablaOuter
                       _ -> error "This can't happen-5"
             f <$> parseExp4

    parseExp6 :: Parser Exp
    parseExp6 = (IntE <$> number)
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
             Negation <$> parseExp6

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
