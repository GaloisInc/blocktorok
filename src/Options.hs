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

import Options.Applicative (Parser, help, metavar, long, short, strOption)

data Options =
  Options { source :: String
          , target :: String
          }

parseOpts :: Parser Options
parseOpts =
  Options <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "The LINK source to be compiled")
          <*> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "The file to output to")
