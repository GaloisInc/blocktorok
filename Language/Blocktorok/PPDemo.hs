{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide       #-}

module Language.Blocktorok.PPDemo where

import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO

import           Prettyprinter              (Doc, Pretty (pretty), (<+>))
import qualified Prettyprinter              as PP

import qualified Language.Blocktorok.BlockM as B
import qualified Language.Blocktorok.Parser as BP

battleFromFile :: FilePath -> FilePath -> IO ()
battleFromFile inf outf =
  do  t <- TIO.readFile inf
      case battle t of
        Left err ->
          TIO.putStrLn ("Error: " <> err)
        Right d ->
          do  TIO.putStrLn "Output complete!"
              writeFile outf (show d)


battle :: Text -> Either Text (Doc ann)
battle s =
  do  b <- BP.parseBlocktorok s
      B.runBlockM b block2doc
  where
    -- building abstractions like this can be a cool feature of the language?

    -- a block containing a creature's stats
    creature :: Text -> B.BlockM (Doc a)
    creature name =
      do  hp <- diceOrFixed "hp"
          dmg <- diceOrFixed "damage"
          pure $ call "Creature" [PP.dquotes $ pretty name, hp, dmg]

    -- a constructor describing either a fixed or random value
    diceOrFixed n = B.valueOf n $
      B.oneConstructorOf  [ ("Dice"
                            , do  sides <- pretty <$> B.valueOf "sides" B.double
                                  number <- pretty <$> B.valueOf "number" B.double
                                  pure $ call "Roll" [sides, number]
                            )
                          , ("Fixed"
                            , do  amount <- pretty <$> B.valueOf "amount" B.double
                                  pure $ call "Fixed" [amount]
                            )
                          ]

    -- generate a call
    call :: Text -> [Doc ann] -> Doc ann
    call n args = pretty n <> PP.parens (PP.sep $ PP.punctuate "," args)

    asList = PP.brackets . PP.hsep . PP.punctuate ","

    -- parse block
    block2doc =
      do  heroes <- B.subBlocks "hero"
            do  name <- B.valueOf "name" B.string
                creature name

          orcs <- B.subBlocks "orc" (creature "Orc")
          minotaurs <- B.subBlocks "minotaur" (creature "Minotaur")

          pure $
            PP.vcat [ "from battle import *"
                    , "heroes" <+> "=" <+> asList heroes
                    , "orcs" <+> "=" <+> asList orcs
                    , "minotaurs" <+> "=" <+> asList minotaurs
                    , call "battle" ["heroes", "orcs" <+> "+" <+> "minotaurs"]
                    ]





