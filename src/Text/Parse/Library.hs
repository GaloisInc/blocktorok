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
  ( parseLib
  ) where

import Control.Monad.Except (Except, withExcept)
import Control.Monad.Trans.Except (except)

import Data.Aeson (eitherDecode')

import Data.Backends.SU2 (SU2Config)

import Data.ByteString.Lazy (ByteString)

import Language.Error (LinkError (LibParseError))

parseLib :: ByteString -> Except LinkError SU2Config
parseLib lib = withExcept LibParseError $ except (eitherDecode' lib :: Either String SU2Config)
