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
  , noLocDoc
  , (</>)
  , (<//>)
  ) where

import           Prettyprinter   (Doc, Pretty (pretty), hardline)

import           Language.Common (Located, unloc)

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
noLocDoc :: Pretty a => Located a -> Doc ann
noLocDoc = pretty . unloc
