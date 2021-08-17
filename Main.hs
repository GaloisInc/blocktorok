{-|
Module      : Main
Description : The LINK compiler entry point
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

The entry point to the LINK compiler. This is likely to be an ever-evolving
module, as we identify new wants/needs in terms of command-line options,
functionality, and behavior.
-}

module Main (main) where

import           Options             (Options (..), parseOpts)
import           Options.Applicative (execParser, fullDesc, header, helper,
                                      info, progDesc, (<**>))

import           Link                (runTransformIO)

-- | Program entrypoint - Consume command line arguments and run the compiler
main :: IO ()
main = realMain =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
      (fullDesc <> progDesc "Transform Blocktorok data." <> header "blocktorok - A Blocktorok data transformer")

realMain :: Options -> IO ()
realMain Options { transformer = t, output = o, blocktorok = d} =
  runTransformIO t d o
