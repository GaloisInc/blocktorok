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
import qualified Data.Map.Strict as M

import Data.Backends.SU2 (SU2Prog(..))
import Data.Class.Render
import Language.Compile.SU2 (compile)
import Language.Error (LinkError)
import Language.Link (link)
import Options
import Text.Parse.Library (parseLib)
import Text.Parse.Link (parseDecl)

import Options.Applicative

import System.Directory (listDirectory)
import System.Exit (exitFailure)

main :: IO ()
main = realMain =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
      (fullDesc <> progDesc "Compile a LINK program" <> header "steel - A LINK compiler")

realMain :: Options -> IO ()
realMain Options { sources = inputs, target = output, libDir = lib } =
  do inputContents <- mapM readFile inputs
     libNames <- listDirectory lib
     let libFullNames = map ((lib ++ "/") ++) libNames
     libContents <- mapM B.readFile libFullNames
     case runExcept $ processProg inputContents libNames libContents of
       Left e -> putStrLn (render e) >> exitFailure
       Right (SU2Prog top rest) ->
         do writeFile output (render top)
            let mNames = zipWith (++) (const "m" <$> rest) (show <$> [1..])
                mOuts  = zip mNames (render <$> rest)
            sequence_ $ (uncurry writeFile) <$> mOuts

  where
    processProg :: [String] -> [String] -> [ByteString] -> Except LinkError SU2Prog
    processProg inputContents libNames libContents =
      do libs <- mapM parseLib libContents
         let namedLibs = M.fromList $ zip libNames libs
         progs <- zipWithM parseDecl inputs inputContents
         prog <- link progs
         compile namedLibs prog
