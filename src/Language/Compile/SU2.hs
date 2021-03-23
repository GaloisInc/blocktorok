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
import Control.Monad.State (StateT, execStateT, get, lift, put)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Backends.SU2
  ( IncScheme(..)
  , GradMethod(..)
  , LinearSolver(..)
  , MathProb(..)
  , MeshFormat(..)
  , Objective(..)
  , Preconditioner(..)
  , SU2Config(..)
  , SU2RHS(..)
  , SU2Solver(..)
  , TabFormat(..)
  )
import Data.Link.AST (Config(..), Prog(..), RunFn(..))
import Data.Link.Identifier (Identifier(..))
import Data.Physics.Model (Model(..), PhysicsType(..), VarSolve(..))
import Data.Solver.Backend (BackendConfig(..), PlotMarkers(..))
import Language.Error (LinkError(..))

-- TODO: Might want this to be generic in the return type, but for now all we care about is the state
type SU2Compiler = StateT SU2Config (Except LinkError) ()

-- TODO: This function should have a _type_ specialized to the SU2 backend rather than throwing
-- an error on non-SU2 - In other words, the decision to call this or another compiler function
-- should be determined earlier. Maybe some type-level magic?
compile :: Map String SU2Config -> Prog -> Except LinkError SU2Config
compile libs (Prog (Config _ _ _ (RFn f _) (Su2 (Identifier fmt) t plot)) models _) =
  execStateT compile' $ SU2Config Map.empty
  where
    compile' :: SU2Compiler
    compile' =
      do compileFmt
         compileTime
         compilePlotting
         compileModel
         compileMagic

    compileFmt :: SU2Compiler
    compileFmt =
      case Map.lookup fmt libs of
        Nothing -> throwError $ UnknownFormat fmt
        Just (SU2Config fmtCfg) ->
          do SU2Config cfg <- get
             put $ SU2Config $ Map.union fmtCfg cfg

    compileTime :: SU2Compiler
    compileTime =
      do SU2Config cfg <- get
         put $ SU2Config $ Map.insert "INNER_ITER" (Integral t) cfg

    compilePlotting :: SU2Compiler
    compilePlotting =
      do SU2Config cfg <- get
         let mplots = case plot of
                        PlotMarkers [] -> Nothing
                        PlotMarkers ps -> Just $ (\(Identifier s) -> s) <$> ps
         put $ SU2Config $ Map.insert "MARKER_PLOTTING" (Markers mplots) cfg

    compileModel :: SU2Compiler
    compileModel =
      do Model _ _ _ _ pType _ _ _ _ (VarSolve _ st ns)  <- lift $ lookupModel f models
         compilePType pType
         compileSolveTechnique st
         compileNumScheme ns

    compilePType :: PhysicsType -> SU2Compiler
    compilePType (HeatConduction (Identifier pParams)) =
      case Map.lookup pParams libs of
        Nothing -> throwError $ UnknownPhysParams pParams
        Just (SU2Config physCfg) ->
          do SU2Config cfg <- get
             put $ SU2Config $ Map.union physCfg cfg
    compilePType pType = throwError $ UnsupportedPhys pType

    compileSolveTechnique :: Identifier -> SU2Compiler
    compileSolveTechnique (Identifier st) =
      case Map.lookup st libs of
        Nothing -> throwError $ UnknownSolvingTech st
        Just (SU2Config stCfg) ->
          do (SU2Config cfg) <- get
             put $ SU2Config $ Map.union stCfg cfg

    compileNumScheme :: Identifier -> SU2Compiler
    compileNumScheme (Identifier ns) =
      case Map.lookup ns libs of
        Nothing -> throwError $ UnknownNumScheme ns
        Just (SU2Config nsCfg) ->
          do (SU2Config cfg) <- get
             put $ SU2Config $ Map.union nsCfg cfg

    compileMagic :: SU2Compiler
    compileMagic =
      do SU2Config cfg <- get
         let magic = Map.fromList $ zip ["TIME_DOMAIN", "TIME_STEP", "MAX_TIME", "TIME_ITER", "MARKER_HEATFLUX", "MARKER_PLOTTING", "SOLID_TEMPERATURE_INIT"]
                                        [Boolean True, Floating 0.005, Integral 3, Integral 600, MarkerData [("top", 0), ("bottom", 0)], Markers $ Just ["left", "right", "top", "bottom"], Floating 273.0]
         put $ SU2Config $ Map.union magic cfg

    lookupModel :: Identifier -> Map Identifier Model -> Except LinkError Model
    lookupModel ident ms =
      case Map.lookup ident ms of
        Nothing -> throwError $ NoModelWithName $ show ident
        Just m  -> return m
compile _ _ = throwError NYI
