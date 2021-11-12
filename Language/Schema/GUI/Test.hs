{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Language.Schema.GUI.Test
Description : A simple test playground for the eventual data input GUI
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

NOTE: Nothing important to see here; just a GUI playground!
-}

module Language.Schema.GUI.Test
  ( showGUI
  ) where

import           Control.Lens

import           Data.Text    (Text)

import           Monomer
import qualified Monomer.Lens as L

import           TextShow

data ListItem = ListItem
  { _ts   :: Int
  , _text :: Text
  } deriving (Eq, Show)

data AppModel = AppModel
  { _newItemText :: Text
  , _items       :: [ListItem]
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AddItem
  | RemoveItem Int
  deriving (Eq, Show)

makeLenses 'ListItem
makeLenses 'AppModel

buildUI :: WidgetEnv AppModel AppEvent
        -> AppModel
        -> WidgetNode AppModel AppEvent
buildUI _ model = widgetTree
  where
    listItem idx item =
      vstack [ label_ (item ^. text) [ellipsis] `styleBasic` [textSize 12, paddingH 8]
             , spacer
             , hstack [ textField (items . singular (ix idx) . text)
                      , spacer
                      , button "Delete" (RemoveItem idx)
                      ]
             ] `nodeKey` showt (item ^. ts) `styleBasic` [paddingT 10]

    widgetTree =
      vstack [ keystroke [("Enter", AddItem)] $ hstack [ label "Description:"
                                                       , spacer
                                                       , textField_ newItemText [placeholder "Write here!"]
                                                       , spacer
                                                       , button "Add" AddItem `styleBasic` [paddingH 5] `nodeEnabled` (model ^. newItemText /= "")
                                                       ]
             , separatorLine `styleBasic` [paddingT 20, paddingB 10]
             , vstack (zipWith listItem [0..] (model ^. items))
             ] `styleBasic` [padding 20]

handleEvent :: WidgetEnv AppModel AppEvent
            -> WidgetNode AppModel AppEvent
            -> AppModel
            -> AppEvent
            -> [AppEventResponse AppModel AppEvent]
handleEvent wenv _ model evt =
  case evt of
    AppInit     -> []
    AddItem
      | model ^. newItemText /= "" -> [Model $ model & newItemText .~ "" & items .~ newItem : model ^. items]
    RemoveItem idx -> [Model $ model & items .~ removeIdx idx (model ^. items)]
    _ -> []
  where
    newItem = ListItem (wenv ^. L.timestamp) (model ^. newItemText)

removeIdx :: Int -> [a] -> [a]
removeIdx idx lst = part1 ++ drop 1 part2
  where
    (part1, part2) = splitAt idx lst

showGUI :: FilePath -> IO ()
showGUI _ = startApp model handleEvent buildUI config
  where
    config = [ appWindowTitle "Test GUI"
             , appTheme darkTheme
             , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
             , appInitEvent AppInit
             ]
    model = AppModel { _newItemText = ""
                     , _items = []
                     }
