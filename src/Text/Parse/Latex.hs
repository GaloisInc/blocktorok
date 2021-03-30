module Text.Parse.Latex(parseLatexExp, parseLatexEquations) where

import Control.Monad((>=>))
import Text.Parsec(choice, try, between, sepBy, chainl1, many1)

import qualified Text.Token as Token
import Text.Token(tok', Parser)
import Text.TokenClass(TokenLatex(..), TokenClass(..))
import Text.Lexer(Token(..))
import qualified Data.LatexSyntax as LS

--
-- NOTE: this document was used to derive a concrete syntax:
--     http://mirrors.ibiblio.org/CTAN/macros/latex/contrib/physics/physics.pdf
--

parseLatexEquations :: Token.Parser [LS.Equation]
parseLatexEquations =
    between (try $ tok' TokenLatexBegin) (tok' TokenLatexEnd) parseLEqs

parseLatexExp :: Parser LS.Exp
parseLatexExp = parseLatexExp5

-------------------------------------------------------------------------------

parseLatexExp0 :: Parser LS.Exp
parseLatexExp0 =
  choice [ parseInt
         , LS.Var <$> parseIdent
         , parseFrac
         , braceOp "\\curl" LS.Curl
         , braceOp "\\div" LS.Divergence
         , braceOp "\\grad" LS.Gradient
         , braceOp "\\sqrt" LS.Sqrt
         , braceOp "\\laplacian" LS.Laplacian
         , inParens parseLatexExp
         ]
  where
  braceOp k c =
    try (sym k) >> inBraces (LS.UnOp c <$> parseLatexExp)

parseLatexExp1 :: Parser LS.Exp
parseLatexExp1 =
  choice [ braced
         , parseLatexExp0 `chainl1` (sym "^" >> pure (LS.BinOp LS.Exponent))
         ]
  where
    braced =
      do  e1 <- try (parseLatexExp0 <* sym "^" <* sym "{")
          e2 <- parseLatexExp
          sym "}"
          pure (LS.BinOp LS.Exponent e1 e2)

-- product
parseLatexExp2 :: Parser LS.Exp
parseLatexExp2 =
  foldl1 (LS.BinOp LS.ScalarProduct) <$> many1 parseLatexExp1

parseLatexExp3 :: Parser LS.Exp
parseLatexExp3 =
  choice [ partialOp
         , parseLatexExp2
         ]
  where
    partialOp =
      do  try (sym "\\frac" >> inBraces (sym "\\partial"))
          wrt <- inBraces (sym "\\partial" >> parseIdent)
          e <- parseLatexExp2
          pure (LS.PartialDerivative e wrt)

-- there does not appear to be a canonical order of operations for these
-- i have made the left associative with the same precedence
parseLatexExp4 :: Parser LS.Exp
parseLatexExp4 = parseLatexExp3 `chainl1` op
  where
    op =
      choice [ sym "\\times" >> pure (LS.BinOp LS.CrossProduct)
             , sym "\\otimes" >> pure (LS.BinOp LS.OuterProduct)
             , sym "\\dot" >> pure (LS.BinOp LS.InnerProduct)
             ]

-- plus/minus
parseLatexExp5 :: Parser LS.Exp
parseLatexExp5 = parseLatexExp4 `chainl1` op
  where
    op =
      choice [ sym "+" >> pure (LS.BinOp LS.Add)
             , sym "-" >> pure (LS.BinOp LS.Subtract)
             ]

parseLEq :: Parser LS.Equation
parseLEq =
  LS.Equation <$> parseLatexExp <* sym "=" <*> parseLatexExp

parseLEqs :: Parser [LS.Equation]
parseLEqs =
  sepBy parseLEq (sym "\\\\")


parseIdent0 :: Parser LS.Identifier
parseIdent0 =
  choice [ try (sym "\\vec") >> LS.Vect <$> inBraces parseIdent
         , LS.Name <$> try ident
         ]

parseIdent1 :: Parser LS.Identifier
parseIdent1 =
  do i0 <- parseIdent0
     choice [ try (sym "_") >> (LS.Subscript i0 <$> parseIdent)
            , pure i0
            ]

parseIdent :: Parser LS.Identifier
parseIdent = parseIdent1

parseInt :: Parser LS.Exp
parseInt = LS.Int <$> try int

-- `try` on this could be a little tighter
parseFrac :: Token.Parser LS.Exp
parseFrac =
  do  try $ sym "\\frac"
      choice [ parDeriv
             , fraction
             ]
  where
    parDeriv =
      do  i1 <- try (inBraces (sym "\\partial" >> parseLatexExp))
          i2 <- inBraces (sym "\\partial" >> parseIdent)
          pure (LS.PartialDerivative i1 i2)
    fraction =
      do  e1 <- inBraces parseLatexExp
          e2 <- inBraces parseLatexExp
          pure (LS.BinOp LS.Divide e1 e2)

inParens :: Parser a -> Parser a
inParens = between (sym "(") (sym ")")

inBraces :: Parser a -> Parser a
inBraces = between (sym "{") (sym "}")

int :: Token.Parser Integer
int = Token.satisfy' (acceptLatex >=> acceptInt)

ident :: Token.Parser String
ident = Token.satisfy' (acceptLatex >=> acceptIdent)

sym :: String -> Parser ()
sym s = Token.satisfy' (acceptLatex >=> acceptSym >=> acceptEq s)

acceptLatex :: Token -> Maybe TokenLatex
acceptLatex (Token _ (TokenLatex tl)) = Just tl
acceptLatex _ = Nothing

acceptSym :: TokenLatex -> Maybe String
acceptSym (TLSymbol s) = Just s
acceptSym _ = Nothing

acceptIdent :: TokenLatex -> Maybe String
acceptIdent (TLIdent i) = Just i
acceptIdent _ = Nothing

acceptInt :: TokenLatex -> Maybe Integer
acceptInt (TLInt i) = Just i
acceptInt _ = Nothing

acceptEq :: String -> String -> Maybe ()
acceptEq  s1 s2 | s1 == s2 = Just ()
                | otherwise = Nothing

