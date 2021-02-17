{-# LANGUAGE OverloadedStrings #-}
module Data.LatexSyntax where

import qualified Prettyprinter as Pretty
import Prettyprinter(pretty, (<+>))

data Op1 =
    Laplacian
  | Divergence
  | Gradient
  | Curl
  | Sqrt
  deriving (Eq, Show)

data Op2 =
    Add
  | Divide
  | Subtract
  | Exponent
  | ScalarProduct
  | InnerProduct
  | OuterProduct
  | CrossProduct
  deriving (Eq, Show)

data Identifier =
    Name String
  | Vect Identifier
  | Subscript Identifier Identifier
  deriving (Eq, Show)

data Exp =
    Var Identifier
  | PartialDerivative Exp Identifier
  | Int Integer
  | UnOp Op1 Exp
  | BinOp Op2 Exp Exp
  deriving (Eq, Show)

data Equation =
  Equation Exp Exp
  deriving (Eq, Show)

instance Pretty.Pretty Identifier where
  pretty i =
    case i of
      Name s -> pretty s
      Vect s -> "\\vec" <> Pretty.brackets (pretty s)
      Subscript i1 i2 -> pretty i1 <> "_" <> pretty i2

-- TODO: precedence might need some work
-- ref: http://www.dfcd.net/articles/latex/latex.html
instance Pretty.Pretty Exp where
  pretty e =
    case e of
      Var v -> pretty v

      Int i -> pretty i

      PartialDerivative (Var i) dt ->
        "\\frac" <> Pretty.brackets ("\\partial" <+> pretty i)
                 <> Pretty.brackets ("\\partial" <+> pretty dt)

      PartialDerivative dx dt ->
        "\\frac" <> Pretty.brackets "\\partial"
                 <> Pretty.brackets ("\\partial" <+> pretty dt)
                 <+> post dx

      UnOp op e1 ->
        case op of
          Laplacian -> "\\Delta" <+> post e1
          Divergence -> "\\div" <+> Pretty.brackets (pretty e1)
          Gradient -> "\\grad" <+>  Pretty.brackets (pretty e1)
          Curl -> "\\curl" <+> Pretty.brackets (pretty e1)
          Sqrt -> "\\sqrt" <> Pretty.brackets (pretty e1)

      BinOp op e1 e2 ->
        case op of
          Add -> pre e1 <+> "+" <+> post e2
          ScalarProduct -> pre e1 <+> post e2
          Divide -> "\\frac" <> Pretty.brackets (pretty e1)
                             <> Pretty.brackets (pretty e2)
          Subtract -> pre e1 <+> "-" <+> post e2
          Exponent -> pre e1 <> "^" <> Pretty.brackets (pretty e2)
          InnerProduct -> pre e1 <+> "\\dot" <+> post e2
          OuterProduct -> pre e1 <+> "\\otimes" <+> post e2
          CrossProduct -> pre e1 <+> "\\times" <+> post e2

    where
      -- we assume everything is left associative here
      pre subExpr =
        if precedence e <= precedence subExpr
          then Pretty.parens (pretty subExpr)
          else pretty subExpr

      post subExpr =
        if precedence e < precedence subExpr
          then Pretty.parens (pretty subExpr)
          else pretty subExpr

      precedence :: Exp -> Int
      precedence e0 =
        case e0 of
          Var _ -> 0
          Int _ -> 0
          PartialDerivative (Var _) _ -> 0
          PartialDerivative _ _ -> 2
          UnOp o _ ->
            case o of
              Curl -> 0
              Divergence -> 0
              Gradient -> 0
              Sqrt -> 0
              Laplacian -> 3
          BinOp o _ _ ->
            case o of
              Add -> 5
              Subtract -> 5
              Exponent -> 1
              ScalarProduct -> 2
              InnerProduct -> 4
              OuterProduct -> 4
              CrossProduct -> 4
              Divide -> 0






