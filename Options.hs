{-|
Module:     : Options
Description : Implementation of Blocktorok command-line options
Copyright   : (c) Galois, Inc. 2021
Liense      : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module defines the command-line options available to the Blocktorok
compiler. It should be treated as ever-evolving as we determine our
needs, wants, and what is useful for demos etc.
-}

module Options
  ( BuildOptions(..)
  , Command(..)
  , DocOptions(..)
  , Options(..)
  , parseBuildOpts
  , parseDocOpts
  , parseOpts
  ) where

import           Options.Applicative (Parser, argument, command, help,
                                      hsubparser, info, long, metavar, progDesc,
                                      short, str, strOption)

-- | The options provided at the command line to control documentation
-- generation
newtype DocOptions = DocOptions { schema :: FilePath }

-- | The options provided at the command line to control compilation
data BuildOptions = BuildOptions
  { transformer :: FilePath
  , output      :: FilePath
  , blocktorok  :: FilePath
  }

-- | Commands recognized at the command line (like git add, git commit, etc)
data Command
  = Doc DocOptions
  | Build BuildOptions

-- | Options provided at the command line, generally (i.e. all known commands)
newtype Options = Options { optCommand :: Command }

-- | A parser for documentation generation command line options
parseDocOpts :: Parser DocOptions
parseDocOpts =
  DocOptions <$> argument str (metavar "FILE" <> help "The schema to generate documentation from")

-- | A parser for build command line options
parseBuildOpts :: Parser BuildOptions
parseBuildOpts =
  BuildOptions <$> strOption (long "transformer" <> short 't' <> metavar "FILE" <> help "The transformer to apply to the input data")
               <*> strOption (long "output" <> short 'o' <> metavar "DIR" <> help "The directory to send outputs to")
               <*> argument str (metavar "FILE" <> help "The data to be transformed")

-- | A parser for all command line options
parseOpts :: Parser Options
parseOpts = Options <$>
  hsubparser
    ( command "doc" (info (Doc <$> parseDocOpts) (progDesc "Generate documentation from a schema"))
   <> command "build" (info (Build <$> parseBuildOpts) (progDesc "Run a transformer on data"))
    )
