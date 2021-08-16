module Link where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Control.Exception as Ex
import qualified Data.Map as Map
import qualified System.FilePath as Path
import qualified System.Directory as Dir


import Language.Transform.Parser(transformFromFile)
import qualified Language.Transform.Syntax as Tx
import qualified Language.Transform.Evaluator as TxEval
import Language.Link.Blocktorok.Parser(elementsFromFile)
import Language.Schema.Parser(schemaFromFile)
import Language.Common(Located(..))
import Data.Foldable(traverse_)

class FormatException a where

orThrow :: Ex.Exception c => IO (Either a b) -> (a -> c) -> IO b
orThrow io mkC  =
  do  eitherB <- io
      case eitherB of
        Left a -> Ex.throwIO (mkC a)
        Right b -> pure b

newtype ParseError = ParseError Text
  deriving(Show)
instance Ex.Exception ParseError where

newtype EvalError = EvalError Text
  deriving(Show)
instance Ex.Exception EvalError where

runTransformIO :: FilePath -> FilePath -> FilePath -> IO ()
runTransformIO txPath blokPath out =
  do  -- load blo
      bloks <- elementsFromFile blokPath `orThrow` ParseError

      -- load transform
      tx <- transformFromFile txPath `orThrow` ParseError
      transformDir <- absDir txPath

      -- load schema
      let schemaRelPath = Text.unpack . locValue $ Tx.transformSchema tx
          schemaPath = rel transformDir schemaRelPath
      schemaEnv <- schemaFromFile schemaPath

      -- run transform
      let eOutputs = TxEval.runTransform schemaEnv tx bloks
      outputs <- pure eOutputs `orThrow` (EvalError . TxEval.showErr)
      print outputs

      -- do output
      Dir.createDirectoryIfMissing True out
      writeOutput `traverse_` Map.toList outputs
  where
    absDir p = Path.takeDirectory <$> Dir.makeAbsolute p
    writeOutput (file, contents) = writeFile (rel out file) (show contents)
    rel p1 p2 | Path.isRelative p2 = p1 Path.</> p2
              | otherwise          = p2





