{-|
Module:     : Options
Description : Implementation of LINK command-line options
Copyright   : (c) Galois, Inc. 2021
Liense      : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module defines the command-line options available to the LINK compiler. It
should be treated as ever-evolving as we determine our needs/wants/what is
useful for demos etc.
-}

module Options
  ( Options(..)
  , parseOpts
  ) where

import Options.Applicative (Parser, argument, help, metavar, long, short, str, strOption)

data Options = Options
  { transformer :: String
  , blocktorok  :: String
  }

parseOpts :: Parser Options
parseOpts =
  Options <$> strOption (long "transformer" <> short 't' <> metavar "FILE" <> help "The transformer to apply to the input data")
          <*> argument str (metavar "FILE" <> help "The data to be transformed")
