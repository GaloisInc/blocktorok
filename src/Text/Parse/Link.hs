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
import Control.Monad((>=>))

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
import Data.Solver.Backend
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

parseSolver:: Parser Solver
parseSolver =
 do
    tok' TokenTsolver
    tok' TokenColon
    s <- tok TokenPCG
    tok' TokenSemi
    return $ case s of
            TokenPCG -> PCG
            _ -> error "This can't happen"

parsePreconditioner:: Parser Preconditioner
parsePreconditioner =
 do
    tok' TokenTpreconditioner
    tok' TokenColon
    p <- tok TokenDIC
    tok' TokenSemi
    return $ case p of
            TokenDIC -> DIC
            _ -> error "This can't happen"

parseDdt :: Parser Ddt
parseDdt =
 do
    tok' TokenNddt
    tok' TokenColon
    d <- tok TokenEuler
    tok' TokenSemi
    return $ case d of
            TokenEuler -> Euler
            _ -> error "This can't happen"

parseDerivkind :: Parser DerivKind
parseDerivkind =
 do
    d <- tok TokenLinear <|> tok TokenOrthogonal
    tok' TokenSemi
    return $ case d of
            TokenLinear -> Linear
            TokenOrthogonal -> Orthogonal
            TokenGauss -> Gauss
            _ -> error "This can't happen"

parseGradKind :: Parser DerivKind
parseGradKind =
 do
    tok' TokenNgrad
    tok' TokenColon
    tok' TokenGauss
    tok' TokenLinear
    tok' TokenSemi
    return $ GaussLinear

parseLaplacianKind :: Parser DerivKind
parseLaplacianKind =
 do
    tok' TokenNlaplacian
    tok' TokenColon
    tok' TokenGauss
    tok' TokenLinear
    tok' TokenOrthogonal
    tok' TokenSemi
    return $ GaussLinearOrthongonal

parseDerivkindDecl setting =
 do
    tok' setting
    tok' TokenColon
    d <- parseDerivkind
    return $ d

parseDerivkindsDecl setting =
 do
    tok' setting
    tok' TokenColon
    d <- (many parseDerivkind)
    return $ d

parseIntSetting setting =
 do
    tok' setting
    tok' TokenColon
    n <- number
    tok' TokenSemi
    return n

parseSolvingTechnique:: Parser SolvingTechnique
parseSolvingTechnique =
  do tok' TokenSolvingTechnique
     name <- parseIdentifier
     tok' TokenLCurl
     s <- parseSolver
     p <- parsePreconditioner
     t <- parseIntSetting TokenTtolerance
     r <- parseIntSetting TokenTrelTol
     tok' TokenRCurl
     return $ SolvingTechnique {
        solver = s,
        preconditioner = p,
        tolerance = t,
        relTol = r
       }

parseNumericalScheme:: Parser NumericalScheme
parseNumericalScheme =
  do tok' TokenNumericalScheme
     name <- parseIdentifier
     tok' TokenLCurl
     d <- parseDdt
     g <- parseGradKind
     l <- parseLaplacianKind
     i <- parseDerivkindDecl TokenNinterpolation
     s <- parseDerivkindDecl TokenNsnGrad
     tok' TokenRCurl
     return $  NumericalScheme  {
        ddt = d,
        grad = g,
        laplacian = l,
        interpolation = i,
        snGrad = s
       }

parseProg :: Parser Prog
parseProg =
  do cfg <- parseConfig
     solvingTechnique <- parseSolvingTechnique
     numericalScheme <- parseNumericalScheme
     models <- parseModels
     Prog cfg solvingTechnique numericalScheme models  <$> parseCouplings

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

parseConfig :: Parser Config
parseConfig =
  do tok' TokenConfig
     tok' TokenLCurl
     timeStep <- parseTimeStepConfig
     duration <- parseDurationConfig
     consts <- parseConstDecls
     runfn <- parseRunFn
     tok' TokenRCurl
     return $ Config timeStep duration consts runfn
  where
    parseTimeStepConfig =
      do tok' TokenTimeStep
         tok' TokenColon
         n <- number
         u <- UP.parseUnit
         tok' TokenSemi
         return (n,u)

    parseDurationConfig =
      do mode <- tok TokenIterations <|> tok TokenTotalTime
         tok' TokenColon
         n <- number
         u <- UP.parseUnit
         tok' TokenSemi
         return $ case mode of
                    TokenIterations -> Iterations n u
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
         name <- parseIdentifier
         i <- parseSingleArg
         model <- parseModelBody i
         return (name, model)

    parseModelBody inputDecl =
      do tok' TokenLCurl
         technique <- parseSettingTechnique
         boundaryDecl <- parseBoundaryDecl
         physType <- parsePhysicsType
         consts <- parseConstDecls
         libs <- parseLibDecls
         vs <- parseVarDecls
         eqs <- parseEqs
         varSolve <- parseVarSolveDecl
         outputDecl <- parseReturnDecl
         tok' TokenRCurl
         return $ mkModel inputDecl outputDecl technique boundaryDecl physType consts libs vs eqs varSolve

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
         vs <- parseVarDecls
         eqs <- choice [parseLatexEquations, parseEqs]
         o <- parseReturnDecl
         tok' TokenRCurl
         return $ Coupling mname ma mb i o vs eqs


-- latex parser
-- Questions:
--    \vec{X} is treated as X - is this okay?
--    X_Y is given the name X_Y - is this okay - there might be collisions?
--    How are other operations represented?
--    Do we want mixed equations (utf-8 and latex together)
parseLatexEquations :: Parser [Equation]
parseLatexEquations =
    between (try $ tok' TokenLatexBegin) (tok' TokenLatexEnd) parseLEqs
  where
    parseLatexExp = parseLatexExp5
    -- parse atoms
    parseLatexExp0 =
      choice [ parseInt
             , parseIdent
             , parseFrac
             , Paran <$> inParens parseLatexExp
             ]

    -- pow
    parseLatexExp1 =
      do  e0 <- parseLatexExp0
          choice [ try (sym "^") >> (Pow e0 <$> parseLatexExp0)
                 , pure e0
                 ]

    -- product
    parseLatexExp2 =
      do  e1 <- parseLatexExp1
          choice [ Times e1 <$> try parseLatexExp
                 , pure e1
                 ]

    -- divergence (other stuff?)
    parseLatexExp3 =
      choice [ try (kw "\\div") >> (NablaDot <$> parseLatexExp)
             , parseLatexExp2
             ]

    -- cross product (other stuff?)
    parseLatexExp4 =
      do  e3 <- parseLatexExp3
          choice [ try (kw "\\times") >> (Times e3 <$> parseLatexExp)
                 ]

    -- plus/minus
    parseLatexExp5 =
      do  e4 <- parseLatexExp4
          choice [ try (sym "+") >> (Plus e4 <$> parseLatexExp)
                 , try (sym "-") >> (Minus e4 <$> parseLatexExp)
                 , pure e4
                 ]

    parseLEq =
      Equation <$> parseLatexExp <* sym "=" <*> parseLatexExp

    parseLEqs =
      sepBy parseLEq (sym "\\\\")

    -- parse an identifier, possibly followed by a subscript
    parseIdentBase :: Parser String
    parseIdentBase =
      do  i <- ident
          mbSubs <- choice [ Just <$> try (sym "_" >> ident)
                           , Just <$> (try (sym "_") >> inBraces parseIdentBase)
                           , pure Nothing
                           ]
          case mbSubs of
            Nothing        -> pure i
            Just subscript -> pure $ i ++ "_" ++ subscript

    -- TODO: vector notation is ignored - is that correct?
    parseIdent =
      Var <$> choice [ try (kw "\\vec") >> inBraces parseIdentBase
                     , parseIdentBase
                     ]


    parseInt = IntE <$> int

    parseFrac :: Parser Exp
    parseFrac =
      do  kw "\\frac"
          e1 <- inBraces parseLatexExp
          e2 <- inBraces parseLatexExp
          pure (Div e1 e2)

    inParens = between (sym "(") (sym ")")
    inBraces = between (sym "{") (sym "}")

    int :: Parser Integer
    int = satisfy' (acceptLatex >=> acceptInt)

    ident :: Parser String
    ident = satisfy' (acceptLatex >=> acceptIdent)
    kw s = satisfy' (acceptLatex >=> acceptIdent >=> acceptEq s)
    sym s = satisfy' (acceptLatex >=> acceptSym >=> acceptEq s)

    acceptLatex (Token _ (TokenLatex tl)) = Just tl
    acceptLatex _ = Nothing
    acceptSym (TLSymbol s) = Just s
    acceptSym _ = Nothing
    acceptIdent (TLSymbol i) = Just i
    acceptIdent _ = Nothing
    acceptInt (TLInt i) = Just i
    acceptInt _ = Nothing
    acceptEq  s1 s2 | s1 == s2 = Just ()
                    | otherwise = Nothing

