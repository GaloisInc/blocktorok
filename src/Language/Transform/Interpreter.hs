module Language.Transform.Interpreter where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State

import qualified Language.Transform.Syntax as TX
import qualified Language.Transform.Value as TV
import Language.Transform.Value(Value)
im


data InterpEnv = InterpEnv
  { envBindings :: Map Text Value
  , envBlockEnv :: [(Located Ident, Value)]
  }

type Ident = Text
type Interp a = Except.ExceptT Text (State )



