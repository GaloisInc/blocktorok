{-# LANGUAGE OverloadedStrings #-}
module Data.LatexSyntax where

import qualified Prettyprinter as Pretty
import Prettyprinter(pretty, (<+>))

data Op1 =
    Laplacian
  | Divergence
  | Gradient
  | Curl
  | Negate  -- this is hard to parse
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
  | PartialDerivative [Identifier] Identifier
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

-- TODO: precedence
-- ref: http://www.dfcd.net/articles/latex/latex.html
instance Pretty.Pretty Exp where
  pretty e =
    case e of
      Var v -> pretty v

      Int i -> pretty i

      PartialDerivative dx dt ->
        "\\frac" <> Pretty.brackets ("\\partial" <+> Pretty.hcat (pretty <$> dx))
                 <> Pretty.brackets ("\\partial" <+> pretty dt)

      UnOp op e1 ->
        case op of
          Laplacian -> "\\Delta" <+> pretty e1
          Divergence -> "\\div" <+> Pretty.brackets (pretty e1)
          Gradient -> "\\grad" <+>  Pretty.brackets (pretty e1)
          Curl -> "\\curl" <+> Pretty.brackets (pretty e1)
          Negate -> "-" <+> pretty e1
          Sqrt -> "\\sqrt" <> Pretty.brackets (pretty e1)

      BinOp op e1 e2 ->
        case op of
          Add -> pretty e1 <+> "+" <+> pretty e2
          ScalarProduct -> pretty e1 <+> pretty e2
          Divide -> "\\frac" <> Pretty.brackets (pretty e1)
                             <> Pretty.brackets (pretty e2)
          Subtract -> pretty e1 <+> "-" <+> pretty e2
          Exponent -> pretty e1 <> "^" <> Pretty.brackets (pretty e2)
          InnerProduct -> pretty e1 <+> "\\dot" <+> pretty e2
          OuterProduct -> pretty e1 <+> "\\otimes" <+> pretty e2
          CrossProduct -> pretty e1 <+> "\\times" <+> pretty e2


