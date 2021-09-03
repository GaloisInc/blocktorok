{-# LANGUAGE LambdaCase #-}

{-|
Module      : Main
Description : Compiler stack entry point
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

The entry point to the Blocktorok compiler. This is likely to be an
ever-evolving module, as we identify new wants/needs in terms of
command-line options, functionality, and behavior.
-}

module Main (main) where

import           Options             (BuildOptions (..), Command (..),
                                      DocOptions (..), Options (..), parseOpts)
import           Options.Applicative (execParser, fullDesc, header, helper,
                                      info, progDesc, (<**>))

import qualified Control.Exception   as Ex
import qualified Data.Text.IO        as TIO
import           Link                (LinkError (..), runTransformIO)
import qualified System.Exit         as Exit

-- | Program entrypoint - Consume command line arguments and run the compiler
main :: IO ()
main = realMain =<< execParser opts
  where
    opts = info (parseOpts <**> helper) $
                fullDesc
             <> progDesc "Transform Blocktorok data."
             <> header "blocktorok - A Blocktorok data transformer"

realMain :: Options -> IO ()
realMain Options { optCommand = cmd } =
  case cmd of
    Doc DocOptions {} -> putStrLn "Not yet implemented" >> Exit.exitFailure
    Build BuildOptions { transformer = t, output = o, blocktorok = d} ->
      runTransformIO t d o
      `Ex.catch`
        \case
          ParseError ex -> TIO.putStrLn ex >> Exit.exitFailure
          EvalError ex  -> TIO.putStrLn ex >> Exit.exitFailure
