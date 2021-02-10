module Main(main) where

import Language.Check (hasAllCouplings, allVarsDeclared)
import Text.Parse.Link ( parseDecl )
import System.Environment ( getArgs )
import System.Exit
import Translation.HighToStr (highToStr)
import Data.List

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parseDecl "<stdin>") getContents
              [f] -> fmap (parseDecl f) (readFile f)
              _   -> error "expected max. 1 argument"
  case result of
    Left e -> do
          print e
          exitFailure
    Right e -> if hasAllCouplings e && allVarsDeclared e then let
                 _ = print e
                 str = highToStr e
                 in
                 mapM_ putStrLn str
               else
                 error "static checks failed"
