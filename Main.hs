module Main(main) where


import Language.Parser ( parseDecl )
import System.Environment ( getArgs )
import System.Exit

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
          Right e->  print e
