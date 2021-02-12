module Main(main) where

import Data.Class.Render
import Language.Check (hasAllCouplings, allVarsDeclared)
import Language.Compile.SU2 (compile)
import Text.Parse.Link ( parseDecl )
import System.Environment ( getArgs )
import System.Exit

dst :: String
dst = "test_cases/heat_transfer_rod/LinkPrototype/su2/out.su2"

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parseDecl "<stdin>") getContents
              [f] -> fmap (parseDecl f) (readFile f)
              _   -> error "expected max. 1 argument"
  case result of
    Left e -> print e >> exitFailure
    Right prog -> if hasAllCouplings prog && allVarsDeclared prog then
                    case compile prog of
                      Left e -> print e >> exitFailure
                      Right su2 -> writeFile dst (render su2)
                  else
                    error "static checks failed"
