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

import           Control.Lens               (makeLenses, (^.))

import           Monomer                    (AppEventResponse,
                                             EventResponse (..), WidgetEnv,
                                             WidgetNode, WidgetRequest (..),
                                             appFontDef, appInitEvent, appTheme,
                                             appWindowTitle, darkTheme,
                                             startApp)

import           Language.Blocktorok.Pretty (writeData)
import           Language.Blocktorok.Syntax (BlockElement (..))
import           Language.Common            (orThrow)
import           Language.Schema.Env        (Env (..))
import           Language.Schema.Parser     (schemaEnvFromFile)

import           Link                       (LinkError (..))

newtype AppModel = AppModel {
  _blocks :: [BlockElement]
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSubmit
  | AppExit
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI :: Env
        -> WidgetEnv AppModel AppEvent
        -> AppModel
        -> WidgetNode AppModel AppEvent
buildUI = error "Unimplemented!"

handleEvent :: FilePath
            -> WidgetEnv AppModel AppEvent
            -> WidgetNode AppModel AppEvent
            -> AppModel
            -> AppEvent
            -> [AppEventResponse AppModel AppEvent]
handleEvent o _ _ model evt =
  case evt of
    AppInit   -> []
    AppSubmit -> [Task writeAndQuit]
    AppExit   -> [Request $ ExitApplication True]
  where
    writeAndQuit :: IO AppEvent
    writeAndQuit =
      do  writeData (model ^. blocks) o
          pure AppExit

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
    model = AppModel { _blocks = [] }
