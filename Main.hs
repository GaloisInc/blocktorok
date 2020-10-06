module Main(main) where

--import Language ( eval )
import Parser ( parseExp )
import System.Environment ( getArgs )
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parseExp "<stdin>") getContents
              [f] -> fmap (parseExp f) (readFile f)
              _   -> error "expected max. 1 argument"
  case result of
          Left e -> do
                print (e)
                exitFailure
          Right _ ->  print "Pass"
