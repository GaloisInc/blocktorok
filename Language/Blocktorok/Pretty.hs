{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Blocktorok.Pretty
Description : Pretty-printing of Blocktorok data
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Pretty-printing of Blocktorok data, primarily used to dump data files from the
schema-generated GUI.
-}

module Language.Blocktorok.Pretty
  ( ppData
  ) where

import           Prettyprinter              (Doc, Pretty (pretty), colon,
                                             concatWith, dquotes, indent,
                                             lbrace, list, rbrace, vcat, (<+>))
import           Prettyprinter.Render.Text  (putDoc)

import           Language.Blocktorok.Syntax (BlockElement (..), Value (..))
import           Language.Common            (unloc)
import           Language.Common.Pretty     ((<//>), (</>))

-- | Write formatted Blocktorok data to the file @input.blok@
ppData :: [BlockElement] -> IO ()
ppData bes =
  do  let d = concatWith (<//>) $ ppBlockElement <$> bes
      putDoc d

-------------------------------------------------------------------------------
-- Generating 'Doc's from 'BlockElement's

ppBlockElement :: BlockElement -> Doc ann
ppBlockElement (BlockElement lident v) =
  pretty (unloc lident) <> colon <+> ppValue v

-- TODO: Units can't be easily pretty-printed, so for now we just ignore them
ppValue :: Value -> Doc ann
ppValue v = case v of
  Number ln _ -> pretty $ show $ unloc ln
  List lvs    -> list $ ppValue <$> unloc lvs
  Block lbes  -> lbrace
             </> indent 2 (vcat $ ppBlockElement <$> unloc lbes)
             </> rbrace
  Tag lt mv   -> pretty (unloc lt) <> maybe "" (\v' -> " " <> ppValue v') mv
  String lt   -> dquotes $ pretty $ unloc lt
