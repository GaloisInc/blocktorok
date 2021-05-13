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
  , parseNamedText
  , table
  ) where

import Control.Monad.Except (Except, withExcept)
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Except (except)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Link.AST (Config(..), Coupling(..), Duration(..), Prog(..), RunFn(..), MeshFileTy(..), TimeDomainTy(..), CoupledSurfacesTy(..))
import Data.Link.Identifier (Identifier(..))
import Text.Lexer (llex)
import qualified Text.Parse.Units as UP
import Text.Token ( Parser, tok, tok', number, variable )
import Text.TokenClass
import Data.Math
import Data.Physics.Model
  ( Boundary(..)
  , BoundaryField(..)
  , BoundaryType(..)
  , Model
  , PhysicsType(..)
  , VarSolve(..)
  , mkModel
  )
import Data.Solver.Technique (Technique(..))
import qualified Data.Units.UnitExp as U
import Data.Units.SymbolTable (SymbolTable, mkSymbolTable)
import Data.Units.SI (siUnits)
import Data.Units.SI.Prefixes (siPrefixes)
import Data.Solver.Backend
  ( BackendConfig(..)
  -- , DerivKind(..)
  -- , Ddt(..)
  -- , NumericalScheme(..)
  , PlotMarkers(..)
  -- , Preconditioner(..)
  -- , Solver(..)
  -- , SolvingTechnique(..)
  )
import Language.Error (LinkError(..))

import Language.Haskell.TH.Syntax (Name)

import Text.Parsec
  ( (<|>)
  , chainl1
  , choice
  , many
  , many1
  , runParserT
  , try
  )
import qualified Text.Parsec as Parsec
import qualified Data.Equation as Eqn
import Text.Parse.Latex(parseLatexEquations)

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

parseNamedText :: Parser a -> String -> String -> Except LinkError a
parseNamedText p n s = withExcept LinkParseError $ except $
  case llex s of
    Left e -> flip runReader table $ runParserT (reportLexError e) () n []
    Right xs -> flip runReader table $ runParserT p () n xs

reportLexError :: String -> Parser a
reportLexError msg = fail ("lexical error: " ++ msg)

-- | Parse an entire LINK script.
parseDecl :: FilePath -> String -> Except LinkError Prog
parseDecl = parseNamedText parseProg

-- parseSolver:: Parser Solver
-- parseSolver =
--  do
--     tok' TokenTsolver
--     tok' TokenColon
--     s <- tok TokenPCG
--     tok' TokenSemi
--     return $ case s of
--             TokenPCG -> PCG
--             _ -> error "This can't happen"

-- parsePreconditioner:: Parser Preconditioner
-- parsePreconditioner =
--  do
--     tok' TokenTpreconditioner
--     tok' TokenColon
--     p <- tok TokenDIC
--     tok' TokenSemi
--     return $ case p of
--             TokenDIC -> DIC
--             _ -> error "This can't happen"

-- parseDdt :: Parser Ddt
-- parseDdt =
--  do
--     tok' TokenNddt
--     tok' TokenColon
--     d <- tok TokenEuler
--     tok' TokenSemi
--     return $ case d of
--             TokenEuler -> Euler
--             _ -> error "This can't happen"

-- parseDerivkind :: Parser DerivKind
-- parseDerivkind =
--  do
--     d <- tok TokenLinear <|> tok TokenOrthogonal
--     tok' TokenSemi
--     return $ case d of
--             TokenLinear -> Linear
--             TokenOrthogonal -> Orthogonal
--             TokenGauss -> Gauss
--             _ -> error "This can't happen"

-- parseGradKind :: Parser DerivKind
-- parseGradKind =
--  do
--     tok' TokenNgrad
--     tok' TokenColon
--     tok' TokenGauss
--     tok' TokenLinear
--     tok' TokenSemi
--     return $ GaussLinear

-- parseLaplacianKind :: Parser DerivKind
-- parseLaplacianKind =
--  do
--     tok' TokenNlaplacian
--     tok' TokenColon
--     tok' TokenGauss
--     tok' TokenLinear
--     tok' TokenOrthogonal
--     tok' TokenSemi
--     return $ GaussLinearOrthongonal

-- parseDerivkindDecl setting =
--  do
--     tok' setting
--     tok' TokenColon
--     d <- parseDerivkind
--     return $ d

-- parseDerivkindsDecl setting =
--  do
--     tok' setting
--     tok' TokenColon
--     d <- (many parseDerivkind)
--     return $ d

-- parseIntSetting setting =
--  do
--     tok' setting
--     tok' TokenColon
--     n <- number
--     tok' TokenSemi
--     return n

-- parseSolvingTechnique:: Parser SolvingTechnique
-- parseSolvingTechnique =
--   do tok' TokenSolvingTechnique
--      name <- parseIdentifier
--      tok' TokenLCurl
--      s <- parseSolver
--      p <- parsePreconditioner
--      t <- parseIntSetting TokenTtolerance
--      r <- parseIntSetting TokenTrelTol
--      tok' TokenRCurl
--      return $ SolvingTechnique {
--         solver = s,
--         preconditioner = p,
--         tolerance = t,
--         relTol = r
--        }

-- parseNumericalScheme:: Parser NumericalScheme
-- parseNumericalScheme =
--   do tok' TokenNumericalScheme
--      name <- parseIdentifier
--      tok' TokenLCurl
--      d <- parseDdt
--      g <- parseGradKind
--      l <- parseLaplacianKind
--      i <- parseDerivkindDecl TokenNinterpolation
--      s <- parseDerivkindDecl TokenNsnGrad
--      tok' TokenRCurl
--      return $  NumericalScheme  {
--         ddt = d,
--         grad = g,
--         laplacian = l,
--         interpolation = i,
--         snGrad = s
--        }


parseTimeDomain :: Parser TimeDomainTy
parseTimeDomain =
  do tok' TokenTimeDomain
     tok' TokenColon
     tok' TokenTransient
     tok' TokenSemi
     return $ Transient


parseProg :: Parser Prog
parseProg =
  do cfg <- parseConfig
     -- solvingTechnique <- parseSolvingTechnique
     -- numericalScheme <- parseNumericalScheme
     models <- parseModels
     -- Prog cfg solvingTechnique numericalScheme models  <$> parseCouplings
     Prog cfg models  <$> parseCouplings

parseRunFn :: Parser RunFn
parseRunFn =
  do tok' TokenRun
     tok' TokenColon
     f <- parseIdentifier
     tok' TokenLParen
     arg <- parseIdentifier
     tok' TokenRParen
     tok' TokenSemi
     return $ RFn f arg

parseMesh :: Parser MeshFileTy
parseMesh =
  do tok' TokenMesh
     tok' TokenColon
     name <- parseIdentifier
     tok' TokenDot
     src <- parseIdentifier
     tok' TokenSemi
     return $ MeshFile name src

parsePlotting :: Parser PlotMarkers
parsePlotting = PlotMarkers <$> inParens markerList
  where
    inParens = Parsec.between (tok' TokenLParen) (tok' TokenRParen)
    markerList = Parsec.sepBy1 parseIdentifier (tok' TokenComma)

parseBackendSu2 :: Parser BackendConfig
parseBackendSu2 =
  do
     tok' TokenSu2
     tok' TokenLCurl
     tok' TokenFormat
     tok' TokenColon
     f <- parseIdentifier
     tok' TokenComma
     tok' TokenSharedParams
     tok' TokenColon
     sp <- parseIdentifier
     tok' TokenComma
     tok' TokenPlotting
     tok' TokenColon
     p <- parsePlotting
     tok' TokenRCurl
     tok' TokenSemi
     return $ Su2 f sp p

parseBackendOpenFoam :: Parser BackendConfig
parseBackendOpenFoam =
   do
      tok' TokenOpenFoam
      tok' TokenSemi
      return $ OpenFoam

parseBackend :: Parser BackendConfig
parseBackend =
  do
    tok' TokenBackend
    tok' TokenColon
    parseBackendOpenFoam <|> parseBackendSu2

parseCouplingIterations :: Parser Integer
parseCouplingIterations =
  do
    tok' TokenIterationsCoupling
    tok' TokenColon
    n <- number
    tok' TokenSemi
    return n

parseInnerIterations :: Parser Integer
parseInnerIterations =
  do
    tok' TokenIterationsInner
    tok' TokenColon
    n <- number
    tok' TokenSemi
    return n

parseConfig :: Parser Config
parseConfig =
  do tok' TokenConfig
     tok' TokenLCurl
     timeDomain <- parseTimeDomain
     timeStep <- parseTimeStepConfig
     duration <- parseDurationConfig
     couplingIterations <- parseCouplingIterations
     consts <- parseConstDecls
     runfn <- parseRunFn
     mesh <- parseMesh
     backend <- parseBackend
     tok' TokenRCurl
     return $ Config timeDomain timeStep duration couplingIterations consts runfn mesh backend
  where
    parseTimeStepConfig =
      do tok' TokenTimeStep
         tok' TokenColon
         n <- number
         u <- UP.parseUnit
         tok' TokenSemi
         return (n,u)

    parseDurationConfig =
      do mode <- tok TokenIterationsTime <|> tok TokenTotalTime
         tok' TokenColon
         n <- number
         u <- UP.parseUnit
         tok' TokenSemi
         return $ case mode of
                    TokenIterationsTime -> IterationsTime n u
                    TokenTotalTime -> TotalTime n u
                    _ -> error "This can't happen"
parseSingleArg :: Parser Identifier
parseSingleArg =
  do
    tok' TokenLParen
    j <-  parseIdentifier
    tok' TokenRParen
    return j

parseModels :: Parser (Map Identifier Model)
parseModels =
  do models <- many1 parseModel
     return $ Map.fromList models
  where
    parseModel =
      do tok' TokenModel
         nm <- parseIdentifier
         i <- parseSingleArg
         model <- parseModelBody i
         return (nm, model)

    parseModelBody inputDecl =
      do tok' TokenLCurl
         technique <- parseSettingTechnique
         innerIterations <- parseInnerIterations
         boundaryDecl <- parseBoundaryDecl
         physType <- parsePhysicsType
         consts <- parseConstDecls
         libs <- parseLibDecls
         vs <- parseVarDecls
         eqs <- parseEqs
         varSolve <- parseVarSolveDecl
         outputDecl <- parseReturnDecl
         tok' TokenRCurl
         return $ mkModel inputDecl outputDecl technique  innerIterations boundaryDecl physType consts libs vs eqs varSolve

parseIdentifier :: Parser Identifier
parseIdentifier =
  do Identifier <$> variable

parseReturnDecl :: Parser Identifier
parseReturnDecl =
   do tok' TokenReturn
      var <- variable
      tok' TokenSemi
      return $ Identifier var

parseVarSolveDecl :: Parser VarSolve
parseVarSolveDecl =
   do tok' TokenSolve
      var <- variable
      tok' TokenWith
      tok' TokenLCurl
      s <- variable
      tok' TokenComma
      n <- variable
      tok' TokenRCurl
      tok' TokenSemi
      return $ VarSolve (Identifier var) (Identifier s) (Identifier n)

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
    ident <- parseIdentifier
    tok' TokenRParen
    tok' TokenSemi
    return (T method ident)


parseBoundaryField :: Parser BoundaryField
parseBoundaryField =
  do tok' TokenLParen
     ident <- parseIdentifier
     tok' TokenComma
     method <- parseBoundaryType
     tok' TokenComma
     n <- number
     tok' TokenRParen
     tok' TokenSemi
     return (BoundaryField ident method  n)

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
      do tok' TokenImport
         i <- parseIdentifier
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


parseEqs :: Parser [Eqn.Equation]
parseEqs =
  choice [ fmap Eqn.LatexEquation <$> parseLatexEquations
         , fmap Eqn.MathEquation <$> parseMathEqs
         ]

parseMathEqs :: Parser [Equation]
parseMathEqs = many parseEq
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
    parseExp1 = parseExp2 `chainl1` return Plus

    parseExp2 :: Parser Exp
    parseExp2 = parseExp3 `chainl1` pMulOp
      where
        pMulOp :: Parser (Exp -> Exp -> Exp)
        pMulOp =
          do op <- choice (tok <$> [TokenTimes, TokenInnerProduct, TokenCrossProduct, TokenOuterProduct, TokenDiv])
             return $ case op of
                        TokenTimes -> Times
                        TokenInnerProduct -> InnerProduct
                        TokenCrossProduct -> CrossProduct
                        TokenOuterProduct -> OuterProduct
                        TokenDiv -> Div
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
                       _ -> error "This can't happen"
             f <$> parseExp4

    parseExp5 :: Parser Exp
    parseExp5 = p <|> parseExp6
      where
        p =
          do op <- choice [tok TokenNablaCross, tok TokenNablaDot, tok TokenNablaOuter]
             let f = case op of
                       TokenNablaCross -> NablaCross
                       TokenNablaDot -> NablaDot
                       TokenNablaOuter -> NablaOuter
                       _ -> error "This can't happen"
             f <$> parseExp5

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

parseCoupledSurfaces :: Parser CoupledSurfacesTy
parseCoupledSurfaces =
  do
    name <- parseIdentifier
    tok' TokenEq
    tok' TokenCoupledSurfaces
    tok' TokenLParen
    x <- parseIdentifier
    tok' TokenComma
    y <- parseIdentifier
    tok' TokenRParen
    return $(CoupledSurfaces name x y)


parseCouplings :: Parser [Coupling]
parseCouplings = many parseCoupling
  where
    parseCoupling =
      do tok' TokenCouple
         mname <- parseIdentifier
         ma <- parseIdentifier
         mb <- parseIdentifier
         i <- parseSingleArg
         tok' TokenLCurl
         cs <- parseCoupledSurfaces
         vs <- parseVarDecls
         eqs <- parseEqs
         o <- parseReturnDecl
         tok' TokenRCurl
         return $ Coupling mname ma mb i o cs vs eqs
