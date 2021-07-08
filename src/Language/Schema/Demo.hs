{-# LANGUAGE OverloadedStrings #-}

module Language.Schema.Demo
  ( showSchema
  ) where

import qualified Data.Text.IO as TIO

import Language.Schema.Parser (schemaFromFile)

showSchema :: FilePath -> IO ()
showSchema fp =
  do res <- schemaFromFile fp
     case res of
       Left err -> TIO.putStrLn err
       Right u -> putStrLn $ show u
