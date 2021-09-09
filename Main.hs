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

import qualified Control.Exception               as Ex

import qualified Data.Text.IO                    as TIO

import           Options                         (BuildOptions (..),
                                                  Command (..), Options (..),
                                                  parseOpts)
import           Options.Applicative             (execParser, fullDesc, header,
                                                  helper, info, progDesc,
                                                  (<**>))

import           System.Directory                (makeAbsolute)
import qualified System.Exit                     as Exit

import           Language.Schema.Pretty.Doc      (ppSchemaDocs)
import           Language.Schema.Pretty.Template (ppSchemaTemplate)
import           Link                            (LinkError (..),
                                                  runTransformIO)

-- | Program entrypoint - Consume command line arguments and run the compiler
main :: IO ()
main = realMain =<< execParser opts
  where
    opts = info (parseOpts <**> helper) $
                fullDesc
             <> progDesc "Work with Blocktorok data."
             <> header "blocktorok - A Blocktorok data transformer suite"

realMain :: Options -> IO ()
realMain Options { optCommand = cmd } =
  case cmd of
    Doc fp -> ppSchemaDocs =<< makeAbsolute fp
    Template fp -> ppSchemaTemplate =<< makeAbsolute fp
    Build BuildOptions { transformer = t, output = o, blocktorok = d} ->
      do absT <- makeAbsolute t
         absO <- makeAbsolute o
         absD <- makeAbsolute d
         runTransformIO absT absD absO

  `Ex.catch`

  \case
    ParseError ex -> TIO.putStrLn ex >> Exit.exitFailure
    EvalError ex  -> TIO.putStrLn ex >> Exit.exitFailure
