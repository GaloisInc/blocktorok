{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-|
Module      : Language.Schema.GUI
Description : Generate a GUI for data input from a schema
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

From a Blocktorok schema, generate a GUI allowing users to enter data
satisfying the schema. Generate a data input file from the GUI's model.
-}

module Language.Schema.GUI
  ( showGUI
  ) where

import           Control.Lens               (makeLenses)

import           Data.Scientific            (scientific)
import           Data.Text                  (Text)

import           Monomer                    (AppEventResponse, CmbPadding (..),
                                             CmbStyleBasic (..),
                                             EventResponse (..), WidgetEnv,
                                             WidgetNode, WidgetRequest (..),
                                             appFontDef, appInitEvent, appTheme,
                                             appWindowTitle, box, button,
                                             darkTheme, hstack, label,
                                             numericField, paddingB, paddingR,
                                             paddingT, separatorLine, spacer,
                                             startApp, textDropdown, textField,
                                             vstack)

import           Language.Blocktorok.Pretty (writeData)
import           Language.Blocktorok.Syntax (BlockElement (..), Value (..))
import           Language.Common            (locUnknown, orThrow)
import           Language.Schema.Env        (Env (..))
import           Language.Schema.Parser     (schemaEnvFromFile)

import           Link                       (LinkError (..))

data AppModel = AppModel
  { _urlBase    :: Text
  , _activation :: Text
  , _batchSize  :: Integer
  , _dataSet    :: Text
  } deriving (Eq, Show)

toBlocks :: AppModel -> [BlockElement]
toBlocks AppModel { _urlBase, _activation, _batchSize, _dataSet } =
  let b  = BlockElement (locUnknown "urlBase") $ String (locUnknown _urlBase)
      a  = BlockElement (locUnknown "activation") $ Tag (locUnknown _activation) Nothing
      bs = BlockElement (locUnknown "batchSize") $ Number (locUnknown $ scientific _batchSize 0) Nothing
      d  = BlockElement (locUnknown "dataSet") $ Tag (locUnknown _dataSet) Nothing
      p  = BlockElement (locUnknown "urlParams") $ Block (locUnknown [a, bs, d])
      n  = BlockElement (locUnknown "neural") $ Block (locUnknown [b, p])
  in  [n]

data AppEvent
  = AppInit
  | Submit
  | Exit
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI :: Env
        -> WidgetEnv AppModel AppEvent
        -> AppModel
        -> WidgetNode AppModel AppEvent
buildUI _ _ _ = widgetTree
  where
    widgetTree =
      vstack [ box $ vstack [ hstack [ label "urlBase:" `styleBasic` [paddingR 20]
                                     , spacer
                                     , textField urlBase
                                     ]
                            , separatorLine `styleBasic` [paddingT 10, paddingB 10]
                            , box $ vstack [ label "urlParams:"
                                           , spacer
                                           , hstack [ label "activation:" `styleBasic` [paddingR 6]
                                                    , spacer
                                                    , textDropdown activation ["Tanh", "Sigmoid", "Linear", "ReLU"]
                                                    ]
                                           , spacer
                                           , hstack [ label "batchSize:" `styleBasic` [paddingR 5]
                                                    , spacer
                                                    , numericField batchSize
                                                    ]
                                           , spacer
                                           , hstack [ label "dataSet:" `styleBasic` [paddingR 20]
                                                    , spacer
                                                    , textDropdown dataSet ["Circle", "Xor", "Gauss", "Spiral"]
                                                    ]
                                           ]
                            ] `styleBasic` [padding 5]
             , separatorLine `styleBasic` [paddingT 10, paddingB 10]
             , button "Export Data" Submit
             ]

handleEvent :: FilePath
            -> WidgetEnv AppModel AppEvent
            -> WidgetNode AppModel AppEvent
            -> AppModel
            -> AppEvent
            -> [AppEventResponse AppModel AppEvent]
handleEvent o _ _ model evt =
  case evt of
    AppInit -> []
    Submit  -> [Task writeAndQuit]
    Exit    -> [Request $ ExitApplication True]
  where
    writeAndQuit :: IO AppEvent
    writeAndQuit =
      do  writeData (toBlocks model) o
          pure Exit

showGUI :: FilePath -> FilePath -> IO ()
showGUI s o =
  do  env <- schemaEnvFromFile s `orThrow` ParseError
      startApp model (handleEvent o) (buildUI env) config
  where
    config = [ appWindowTitle "Data Entry"
             , appTheme darkTheme
             , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
             , appInitEvent AppInit
             ]
    model = AppModel { _urlBase = ""
                     , _activation = ""
                     , _batchSize = 0
                     , _dataSet = ""
                     }
