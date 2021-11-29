{-|
Module      : Language.Schema.GUI
Description : Generate a GUI for data input from a schema
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

From a Blocktorok schema, generate a GUI allowing users to enter data
satisfying the schema. Generate a data input file from the GUI's model.
-}

module Language.Schema.GUI
  ( showGUI
  ) where

import           Control.Lens               ()

import           Monomer                    ()

import           Language.Blocktorok.Syntax ()

showGUI :: FilePath -> FilePath -> IO ()
showGUI = undefined
