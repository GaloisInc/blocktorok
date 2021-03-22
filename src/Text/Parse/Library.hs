{-|
Module      : Text.Parse.Library
Description : Parser for LINK backend libraries
Copyright   : Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : Experimental
Portability : N/A

This module defines parsing for backend libraries, which are (in general)
simple JSON objects.

Future versions of LINK will attempt to synthesize Haskell types from the JSON,
improving the type-safety of LINK programs despite the untyped nature of JSON
data.
-}

module Text.Parse.Library
  (
  ) where

import Data.Aeson
