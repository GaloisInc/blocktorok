{-|
Module      : Language.Compile.SU2
Description : Translation from the LINK AST to the SU2 lower IR
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

This module provides a translation between the LINK abstract syntax and SU2
configurations.
-}

module Language.Compile.SU2
  ( compile
  ) where

import Control.Monad.Except (Except, throwError)
import Control.Monad.State (StateT, execStateT, get, put)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Coerce

import Data.Backends.SU2
  ( MeshFormat(..)
  , SU2Prog(..)
  , SU2Config(..)
  , SU2RHS(..)
  , SU2Solver(..)
  )
import Data.Link.AST (Config(..), Duration(..), Prog(..), TimeDomainTy(..))
import Data.Link.Identifier (Identifier(..))
import Data.Physics.Model (Boundary(..), Model(..), PhysicsType(..), VarSolve(..))
import Data.Solver.Backend (BackendConfig(..))
import Language.Error (LinkError(..))

-- TODO: Might want this to be generic in the return type, but for now all we care about is the state
type SU2Compiler a = StateT SU2Prog (Except LinkError) a

-- TODO: This function should have a _type_ specialized to the SU2 backend rather than throwing
-- an error on non-SU2 - In other words, the decision to call this or another compiler function
-- should be determined earlier. Maybe some type-level magic?
compile :: Map String SU2Config -> Prog -> Except LinkError SU2Prog
compile libs (Prog (Config td (step, _) timeIter mci _ _ (Su2 fmt shared gridD)) models _) =
  execStateT compile' $ SU2Prog (SU2Config Map.empty) []
  where
    compile' :: SU2Compiler ()
    compile' =
      do compileTopLevel
         sequence_ $ compileModel <$> models
         let mNames = Markers $ Just $ zipWith (\m n -> m ++ n ++ ".cfg") (const "m" <$> Map.toList models) (show <$> [1..])
         SU2Prog (SU2Config topCfg) rest <- get
         put $ SU2Prog (SU2Config $ Map.insert "CONFIG_LIST" mNames topCfg) rest

    compileTopLevel :: SU2Compiler ()
    compileTopLevel =
      do SU2Prog (SU2Config topCfg) rest <- get
         let iters = case timeIter of {IterationsTime d _ -> round d; TotalTime d _ -> round d}
             opts = Map.fromList $ zip ["TIME_DOMAIN", "TIME_STEP", "TIME_ITER", "SOLVER"]
                                       [Boolean $ case td of { Transient -> True; Steady -> False }, Floating step, Integral iters, Solver Multi]
             opts' = case mci of
                       Nothing -> opts
                       Just ci -> Map.insert "OUTER_ITER" (Integral ci) opts
         put $ SU2Prog (SU2Config $ Map.union topCfg opts') rest
         compileTopLib fmt
         compileTopLib shared
         compileTopLib gridD

    compileTopLib :: Identifier -> SU2Compiler ()
    compileTopLib (Identifier i) =
      case Map.lookup i libs of
        Nothing -> throwError $ UnknownLib i
        Just (SU2Config iCfg) ->
          do SU2Prog (SU2Config cfg) rest <- get
             put $ SU2Prog (SU2Config $ Map.union iCfg cfg) rest

    compileModel :: Model -> SU2Compiler ()
    compileModel (Model _ _ _ innerIt (Identifier mesh) (BoundaryLibs bLibs) pType _ _ _ _ (VarSolve _ libs')) =
      do let cfg = Map.fromList $ zip ["INNER_ITER", "MESH_FILENAME", "MESH_FORMAT"]
                                      [Integral innerIt, Filename mesh, MeshFormat SU2]
         bLibCfgs <- sequence $ compileLib <$> bLibs
         physCfg <- compilePType pType
         libCfgs <- sequence $ compileLib <$> libs'
         SU2Prog top rest <- get
         put $ SU2Prog top ((SU2Config $ foldr Map.union cfg (coerce <$> physCfg:(bLibCfgs ++ libCfgs))) : rest)

    compileLib :: Identifier -> SU2Compiler SU2Config
    compileLib (Identifier i) =
      case Map.lookup i libs of
        Nothing -> throwError $ UnknownLib i
        Just cfg -> return cfg

    compilePType :: PhysicsType -> SU2Compiler SU2Config
    compilePType pType =
      -- We don't actually care about anything but the library references
      let pParams = case pType of { HeatConduction (Identifier p) -> p; FluidFlow (Identifier p) -> p; HeatTransfer (Identifier p) -> p}
      in case Map.lookup pParams libs of
           Nothing -> throwError $ UnknownLib pParams
           Just cfg -> return cfg
compile _ _ = throwError NYI
