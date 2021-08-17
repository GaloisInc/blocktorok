module Link(runTransformIO, ParseError(..), EvalError(..)) where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Control.Exception as Ex
import qualified Data.Map as Map
import qualified System.FilePath as Path
import qualified System.Directory as Dir


import Language.Transform.Parser(transformFromFile)
import qualified Language.Transform.Syntax as Tx
import qualified Language.Transform.Evaluator as TxEval
import qualified Language.Transform.Value as TxValue
import Language.Link.Blocktorok.Parser(elementsFromFile)
import Language.Schema.Parser(schemaFromFile)
import Language.Common(Located(..))
import Data.Foldable(traverse_)

-- |run a transform and produce output
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
          schemaEnv <- schemaFromFile schemaPath

          -- validate and typecheck input
          root <- TxValue.validateElts schemaEnv bloks `orThrow'` EvalError

          -- run transform
          let eOutputs = TxEval.runTransform tx root
          outputs <- eOutputs `orThrow'` EvalError
          print outputs

          -- do output
          Dir.createDirectoryIfMissing True out
          writeOutput `traverse_` Map.toList outputs

    absDir p = Path.takeDirectory <$> Dir.makeAbsolute p
    writeOutput (file, contents) = writeFile (rel out file) (show contents)
    rel p1 p2 | Path.isRelative p2 = p1 Path.</> p2
              | otherwise          = p2


-------------------------------------------------------------------------------

-- |The type of parse errors
newtype ParseError = ParseError Text
  deriving(Show)
instance Ex.Exception ParseError where

-- |The type of evaluation errors
newtype EvalError = EvalError Text
  deriving(Show)
instance Ex.Exception EvalError where


-- internal

orThrow :: Ex.Exception c => IO (Either a b) -> (a -> c) -> IO b
orThrow io mkC  =
  do  eitherB <- io
      case eitherB of
        Left a -> Ex.throwIO (mkC a)
        Right b -> pure b

orThrow' :: Ex.Exception c => Either a b -> (a -> c) -> IO b
orThrow' e  = orThrow (pure e)


