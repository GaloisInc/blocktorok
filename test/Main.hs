module Main where

import Test.Tasty(TestTree(..), defaultMain)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import Test.Tasty.HUnit((@=?))
import Prettyprinter(Pretty(..), pretty)

import Text.Token(Parser(..))
import qualified Text.Parse.Link as LinkParse
import qualified Text.Parse.Latex as LatexParse
import qualified Data.LatexSyntax as Latex
import qualified Text.TokenClass as TC

main :: IO ()
main =
  defaultMain allTests

allTests :: TestTree
allTests =
  Tasty.testGroup "All tests"
    [ latexParserTests
    ]

-------------------------------------------------------------------------------
-- Latex parser tests

latexParserTests :: TestTree
latexParserTests =
  Tasty.testGroup "Latex parser"
    [ Tasty.testGroup "Expressions" (uncurry assertLatexExpParse <$> exps)
    ]

assertLatexExpParse :: String -> Latex.Exp -> Tasty.TestTree
assertLatexExpParse s e =
  HUnit.testCase ("Parse expression: " ++ s) doParse
  where
    doParse =
      do  let eqnStr = asEqnBlock [s ++ " = 0"]     -- nest the eqn in a block
              eqn = Latex.Equation e (Latex.Int 0)  -- to simplify parsing
          parsed <- runParser LatexParse.parseLatexEquations eqnStr
          [eqn] @=? parsed

exps :: [(String, Latex.Exp)]
exps =
  [ ("5", int 5)
  , ("a", a)
  , ("\\gamma", name "\\gamma")
  , ("\\curl{a}", curl a)
  , ("\\div{a}", diver a)
  , ("\\grad{a}", grad a)
  , ("\\sqrt{a}", sqr a)
  , ("(a)", name "a")
  , ("a^2", a `pow` int 2)
  , ("a^{b + 1}", a `pow` (b `plus` int 1))
  , ("a b", a `mul` b)
  , ("\\Delta a", lap a)
  , ("a \\times b", a `crossProd` b)
  , ("a \\otimes b", a `outerProd` b)
  , ("a \\dot b", a `innerProd` b)
  , ("a + b", a `plus` b)
  , ("a - b", a `minus` b)
  ]
  where
    a = name "a"
    b = name "b"
    int = Latex.Int
    name = Latex.Var . Latex.Name
    plus = Latex.BinOp Latex.Add
    minus = Latex.BinOp Latex.Subtract
    curl = Latex.UnOp Latex.Curl
    diver = Latex.UnOp Latex.Divergence
    grad = Latex.UnOp Latex.Gradient
    sqr = Latex.UnOp Latex.Sqrt
    pow = Latex.BinOp Latex.Exponent
    mul = Latex.BinOp Latex.ScalarProduct
    lap = Latex.UnOp Latex.Laplacian
    crossProd = Latex.BinOp Latex.CrossProduct
    innerProd = Latex.BinOp Latex.InnerProduct
    outerProd = Latex.BinOp Latex.OuterProduct




-- latexExpRoundTripTests :: TestTree
-- latexExpRoundTripTests =
--   Tasty.testGroup "Latex round trip tests" $
--     tc <$> [ "a = 0"
--            , "b = 0"
--            , "c = a + b"
--            ]
--   where
--     tc name s =
--       HUnit.testCase ("Roundtrip: " ++ name) (assertRoundTrip s LatexParse.parseLatexExp)

runParser :: Parser a -> String -> IO a
runParser p s =
  case LinkParse.parseNamedText p "<unit test>" s of
    Left err -> HUnit.assertFailure (show err)
    Right a -> pure a

asEqnBlock :: [String] -> String
asEqnBlock eqns =
  unlines $
    [TC.unLex TC.TokenLatexBegin] ++
    punct "  \\\\" eqns ++
    [TC.unLex TC.TokenLatexEnd]
  where
    punct _ []  = []
    punct _ [a] = [a]
    punct s (e:r) = (e ++ s):punct s r

assertRoundTrip parse s0 =
  do  a1 <- parse s0
      let s1 = pretty a1

      a2 <- parse (show s1)
      let s2 = pretty a2

      HUnit.assertEqual "round trip" (show s1) (show s2)

debugParseEqns :: [String] -> IO [Latex.Equation]
debugParseEqns s =
  runParser LatexParse.parseLatexEquations (asEqnBlock s)