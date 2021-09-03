{-|
Module      : Link
Description : Entry to the Blocktorok compiler
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

The 'real' entry to the Blocktorok compiler which parses everything, validates
data against the schema, and runs the transformer to produce output.
-}

module Link
  ( -- * Invoking the compiler
    -- ** Error types
    LinkError(..)
    -- ** Running the transformer
  , runTransformIO
  ) where

import qualified Control.Exception            as Ex

import           Data.Foldable                (traverse_)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

import qualified System.Directory             as Dir
import qualified System.FilePath              as Path

import           Language.Blocktorok.Parser   (elementsFromFile)
import           Language.Common              (Located (..))
import           Language.Schema.Parser       (schemaFromFile)
import qualified Language.Transform.Evaluator as TxEval
import           Language.Transform.Parser    (transformFromFile)
import qualified Language.Transform.Syntax    as Tx
import qualified Language.Transform.Value     as TxValue

-- | run a transform and produce output
-- Throws `ParseError` on failure to parse
-- Throws `EvalError` for errors during validation/evaluation
runTransformIO ::
  -- | Path to the transform file
  FilePath ->

  -- | Path to the blocktorok input file
  FilePath ->

  -- | Directory to output files to (relative paths are relative to this)
  FilePath ->

  IO ()
runTransformIO txPath blokPath out = runTx
  where
    runTx =
      do  -- load blocktorok
          bloks <- elementsFromFile blokPath `orThrow` ParseError

          -- load transform
          tx <- transformFromFile txPath `orThrow` ParseError
          transformDir <- absDir txPath

          -- load schema
          let schemaRelPath = Text.unpack . locValue $ Tx.transformSchema tx
              schemaPath = rel transformDir schemaRelPath
          schemaEnv <- schemaFromFile schemaPath `orThrow` ParseError


          -- validate and typecheck input
          root <- TxValue.validateElts schemaEnv bloks `orThrow'` EvalError

          -- run transform
          let eOutputs = TxEval.runTransform tx root
          outputs <- eOutputs `orThrow'` EvalError

          -- do output
          Dir.createDirectoryIfMissing True out
          writeOutput `traverse_` Map.toList outputs

    absDir p = Path.takeDirectory <$> Dir.makeAbsolute p
    writeOutput (file, contents) =
      do Dir.createDirectoryIfMissing True (rel out $ Path.takeDirectory file)
         writeFile (rel out file) (show contents)
    rel p1 p2 | Path.isRelative p2 = p1 Path.</> p2
              | otherwise          = p2


-------------------------------------------------------------------------------


data LinkError =
    ParseError Text
  | EvalError Text
  deriving(Show)


instance Ex.Exception LinkError where

-- internal

orThrow :: Ex.Exception c => IO (Either a b) -> (a -> c) -> IO b
orThrow io mkC  =
  do  eitherB <- io
      case eitherB of
        Left a  -> Ex.throwIO (mkC a)
        Right b -> pure b

orThrow' :: Ex.Exception c => Either a b -> (a -> c) -> IO b
orThrow' e = orThrow (pure e)
