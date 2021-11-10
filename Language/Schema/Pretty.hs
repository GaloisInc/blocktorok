{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Schema.Pretty
Description : Common pretty-printing combinators
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Some pretty-printing combinators / primitives useful in both documentation and
template generation.
-}

module Language.Schema.Pretty
  ( blankLine
  , ppStype
  , ppNoLoc
  , (</>)
  , (<//>)
  ) where

import           Prettyprinter        (Doc, Pretty (pretty), hardline, (<+>))

import           Language.Common      (Located, unloc)
import           Language.Schema.Type (SType (..))

-- | Combine two 'Doc' @ann@, separating with 'hardline'
(</>) :: Doc ann -> Doc ann -> Doc ann
x </> y = x <> hardline <> y

-- | Combine two 'Doc' @ann@, separating with 'blankLine'
(<//>) :: Doc ann -> Doc ann -> Doc ann
x <//> y = x <> blankLine <> y

-- | Convenient shorthand for two 'hardline's
blankLine :: Doc ann
blankLine = hardline <> hardline

-- | Pretty-print a 'Located' value, ignoring the location data
ppNoLoc :: Pretty a => Located a -> Doc ann
ppNoLoc = pretty . unloc

-- | Pretty-print an 'SType'
ppStype :: SType -> Doc ann
ppStype t =
  case t of
    SInt        -> "int"
    SFloat mu   ->
      "float" <> case mu of { Nothing -> ""; Just u -> " with dimension of" <+> pretty u }
    SString     -> "string"
    SBool       -> "bool"
    SNamed txt  -> pretty txt
