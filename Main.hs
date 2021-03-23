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

import Control.Monad (zipWithM)
import Control.Monad.Except (Except, runExcept)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Data.Backends.SU2 (SU2Config)
import Data.Class.Render
import Language.Compile.SU2 (compile)
import Language.Error (LinkError)
import Language.Link (link)
import Options
import Text.Parse.Link (parseDecl)

import Options.Applicative

import System.Directory (listDirectory)
import System.Exit

main :: IO ()
main = realMain =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
      (fullDesc <> progDesc "Compile a LINK program" <> header "steel - A LINK compiler")

realMain :: Options -> IO ()
realMain Options { sources = inputs, target = output, libDir = lib } =
  do inputContents <- mapM readFile inputs
     libs <- listDirectory lib
     libsContents <- mapM B.readFile libs
     case runExcept $ processProg inputContents libsContents of
       Left e -> putStrLn (render e) >> exitFailure
       Right su2 -> writeFile output (render su2)
  where
    processProg :: [String] -> [ByteString] -> Except LinkError SU2Config
    processProg inputContents libsContents =
      do progs <- zipWithM parseDecl inputs inputContents
         prog <- link progs
         compile prog
