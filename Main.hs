module Main(main) where

import Language.Check (hasAllCouplings, allVarsDeclared)
import Text.Parse.Link ( parseDecl )
import System.Environment ( getArgs )
import System.Exit
import Translation.HighToLow ( highToLow )
import Translation.LowToString ( lowToString )

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
    Right e -> if hasAllCouplings e && allVarsDeclared e then
                 --print e
                 print (lowToString (highToLow e))

               else
                 error "static checks failed"
