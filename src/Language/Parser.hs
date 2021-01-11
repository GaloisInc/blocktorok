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

import Language.AST
import Language.Identifier
import Language.Lexer
import Language.Token
import Language.TokenClass

import Math

import Physics.Model

import Text.Parsec

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
parseProg = do cfg <- parseConfig
               models <- parseModels
               couplings <- parseCouplings
               return $ Prog cfg models couplings

parseConfig :: Parser Config
parseConfig = do tok' TokenConfig
                 tok' TokenLCurl
                 steps <- parseStepConfig
                 duration <- parseDurationConfig
                 tok' TokenRCurl
                 return $ Config steps duration

parseStepConfig :: Parser Int
parseStepConfig = do tok' TokenStep
                     tok' TokenColon
                     n <- number
                     tok' TokenSemi
                     return n

parseDurationConfig :: Parser Duration
parseDurationConfig = do mode <- tok TokenIterations <|> tok TokenTotalTime
                         tok' TokenColon
                         n <- number
                         tok' TokenSemi
                         return $ case mode of
                                    TokenIterations -> Iterations n
                                    TokenTotalTime -> TotalTime n

parseModels :: Parser (Map Identifier Model)
parseModels = do models <- many1 parseModel
                 return $ Map.fromList models

parseModel :: Parser (Identifier, Model)
parseModel = do tok' TokenModel
                id <- parseIdentifier
                model <- parseModelBody
                return (id, model)

parseIdentifier :: Parser Identifier
parseIdentifier = do Identifier <$> variable

parseModelBody :: Parser Model
parseModelBody = do tok' TokenLCurl
                    inputDecl <- parseInputDecl
                    outputDecl <- parseOutputDecl
                    technique <- parseSettingTechnique
                    boundaryDecl <- parseBoundaryDecl
                    physType <- parsePhysicsType
                    consts <- parseConstDecls
                    libs <- parseLibDecls
                    vars <- parseVarDecls
                    eqs <- parseEqs
                    tok' TokenRCurl
                    return $ mkModel inputDecl outputDecl technique boundaryDecl physType consts libs vars eqs

parseInputDecl = undefined

parseOutputDecl = undefined

parseSettingTechnique = undefined

parseBoundaryDecl = undefined

parsePhysicsType = undefined

parseConstDecls = undefined

parseLibDecls = undefined

parseVarDecls = undefined

parseEqs = undefined

parseCouplings = undefined

