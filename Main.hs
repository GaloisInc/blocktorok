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

import Data.Class.Render
import Language.Check (hasAllCouplings, allVarsDeclared)
import Language.Compile.SU2 (compile)
import Options
import Text.Parse.Link (parseDecl)

import Options.Applicative

import System.Exit

main :: IO ()
main = realMain =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
      (fullDesc <> progDesc "Compile a LINK program" <> header "steel - A LINK compiler")

-- TODO: Deal with these nested case expressions. ExceptT?
realMain :: Options -> IO ()
realMain Options { sources = inputs, target = output } =
  do inputContents <- mapM readFile inputs
     let parseResult = zipWithM parseDecl inputs inputContents
     case parseResult of
       Left e -> print e >> exitFailure
       Right progs -> -- TODO: Link all of the parsed LINK code into one large instance of the AST
         if all hasAllCouplings progs && all allVarsDeclared progs then
           case compile (head progs) of
             Left e -> print e >> exitFailure
             Right su2 -> writeFile output (render su2)
         else
           error "Basic static analysis failed (do you have all couplings and are all variables declared before use?)"
