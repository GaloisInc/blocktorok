module Main(main) where

import Data.Class.Render
import Language.Check (hasAllCouplings, allVarsDeclared)
import Language.Compile.SU2 (compile)
import Options
import Text.Parse.Link ( parseDecl )

import Options.Applicative

import System.Exit

main :: IO ()
main = realMain =<< execParser opts
  where
    opts = info (parseOpts <**> helper)
      ( fullDesc <> progDesc "Compile a LINK program" <> header "steel - A LINK compiler")

-- TODO: Deal with these nested case expressions. ExceptT?
realMain :: Options -> IO ()
realMain (Options input output) =
  do parseRes <- parseDecl input <$> readFile input
     case parseRes of
       Left e -> print e >> exitFailure
       Right prog -> if hasAllCouplings prog && allVarsDeclared prog then
                       case compile prog of
                         Left e -> print e >> exitFailure
                         Right su2 -> writeFile output (render su2)
                     else
                       error "Basic static analysis failed (do you have all couplings and are all variables declared before use?)"
