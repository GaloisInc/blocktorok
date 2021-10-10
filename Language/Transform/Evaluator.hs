{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Language.Transform.Evaluator
Description : Blocktorok transformer language evaluator
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

An evaluator for the Blocktorok transformer language, which applies the
transform to real data. This module does not provide the mechanisms to output
the result of applying the transform to files; see "Link" for that interface.
-}

module Language.Transform.Evaluator
  ( describeValueType
  , runTransform
  ) where

import           Control.Applicative       ((<|>))
import qualified Control.Monad.Except      as Except
import qualified Control.Monad.State       as State

import           Data.Foldable             (traverse_)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import qualified Prettyprinter             as PP

import           Language.Common           (HasLocation (..), Located (..),
                                            msgWithLoc, ppRange, unloc)
import qualified Language.Transform.Syntax as Tx
import           Language.Transform.Value  (Value (..))
import qualified Language.Transform.Value  as Value

data InterpEnv = InterpEnv
  { envBindings :: Map Ident Value
  , envBlockEnv :: Map Ident Value
  , envOutputs  :: Map FilePath Doc
  }

type Ident = Text
type Doc = PP.Doc ()

-------------------------------------------------------------------------------
-- Eval monad

type Eval a = Except.ExceptT Text (State.State InterpEnv) a

throw :: HasLocation why => why -> Text -> Eval a
throw why msg = Except.throwError (msgWithLoc why msg)

getVarValMb :: Located Ident -> Eval (Maybe Value)
getVarValMb i =
  do  bindVals <- State.gets (Map.lookup (unloc i) . envBindings)
      envVals <- State.gets (Map.lookup (unloc i) . envBlockEnv)
      pure (envVals <|> bindVals)

getVarVal :: Located Ident -> Eval Value
getVarVal i =
  do  mbVals <- getVarValMb i
      case mbVals of
        Nothing -> throw i (q (unloc i) <> " is not defined here")
        Just a  -> pure a

bindVar :: Located Ident -> Value -> Eval ()
bindVar i v =
  do  mbVals <- getVarValMb i
      case mbVals of
        Nothing ->
            State.modify $ \s ->
              s { envBindings = Map.insert (unloc i) v (envBindings s)
                }
        Just _ ->
          throw i (q (unloc i) <> " is already bound here")

appendToFile :: FilePath -> Doc -> Eval ()
appendToFile fp doc =
  do  out <- State.gets envOutputs
      let doc' =
            case Map.lookup fp out of
              Nothing -> doc
              Just d  -> PP.vcat [d, doc]
      State.modify $ \s -> s { envOutputs = Map.insert fp doc' (envOutputs s) }


modTraverseBlockEnv :: (Value -> Eval Value) -> Eval ()
modTraverseBlockEnv f  =
  do  env <- State.gets envBlockEnv
      env' <- f `traverse` env
      State.modify' (\s -> s { envBlockEnv = env'})

withBlockEnv :: Map Ident Value -> Eval a -> Eval a
withBlockEnv env' eval =
  do  env <- State.gets envBlockEnv
      State.modify (\s -> s { envBlockEnv = env'})
      result <- eval
      State.modify (\s -> s { envBlockEnv = env})
      pure result

scoped :: Eval a -> Eval a
scoped eval =
  do  vars <- State.gets envBindings
      result <- eval
      State.modify (\s -> s { envBindings = vars})
      pure result


-------------------------------------------------------------------------------
-- Evaluator


showValue :: Value -> Eval Doc
showValue v0 =
  case v0 of
    VBool _ True -> pure "true"
    VBool _ False -> pure "false"
    VDouble _ d -> pure $ PP.pretty d
    VInt _ i -> pure $ PP.pretty i
    VString _ s -> pure $ PP.pretty s
    VList _ l -> PP.hcat <$> showValue `traverse` l
    VDoc _ d -> pure d
    VFile _ p -> pure $ PP.pretty p
    VTag c ->
      case Value.tagRenderer c of
        Nothing -> throw v0 ("No renderer for tag at " <> ppRange (Value.tagLoc c))
        Just renderer ->
          case Value.tagValue c of
            Nothing -> showEnv Map.empty renderer
            Just (VBlock b) -> showEnv (Value.blockValues b) renderer
            Just v -> showEnv (Map.singleton "value" v) renderer
    VBlock b ->
      case Value.blockRenderer b of
        Nothing -> throw v0 ("No renderer for block at " <> ppRange (Value.blockLoc b))
        Just r -> showEnv (Value.blockValues b) r
  where
    showEnv env e = withBlockEnv env (evalExpr e) >>= showValue
evalDecl :: Tx.Decl -> Eval ()
evalDecl d0 =
  case d0 of
    Tx.DeclLet i e ->
      do  v <- evalExpr e
          bindVar i v
    Tx.DeclRender s e -> modTraverseBlockEnv (evalRender s e)
    Tx.DeclFileOut f e ->
      do  f' <- getVarVal f >>= file
          doc <- evalExpr e >>= showValue
          appendToFile f' doc
    Tx.DeclIn _ sel decls ->
      do  envs <- evalSelector sel >>= asList envValue
          evalIn decls `traverse_` envs
  where
    evalIn decls env =
      scoped $ withBlockEnv env (evalDecl `traverse_` decls)


evalRender :: Tx.Selector -> Tx.Expr -> Value -> Eval Value
evalRender s0 e = Value.traverseSchemaValues (Value.mapSelected render path) schema
  where
    render vr =
      case vr of
        VBlock vb ->
            pure $ VBlock vb { Value.blockRenderer = Just e}
        VTag vc -> pure $ VTag vc { Value.tagRenderer = Just e }
        _ -> pure vr

    path = reverse (pathRev s0)
    schema = schemaS s0

    pathRev s =
      case s of
        Tx.SelName _     -> []
        Tx.SelMem s' mem -> unloc mem:pathRev s'
    schemaS s =
      case s of
        Tx.SelName n   -> unloc n
        Tx.SelMem s' _ -> schemaS s'

evalExpr :: Tx.Expr -> Eval Value
evalExpr e0 =
  case e0 of
    Tx.ExprFn lc -> evalCall lc
    Tx.ExprSelector s -> evalSelector s
    Tx.ExprLit l ->
      pure $
        case l of
          Tx.LitFloat f  -> VDouble (location f) (unloc f)
          Tx.LitInt i    -> VInt (location i) (unloc i)
          Tx.LitString s -> VString (location s) (unloc s)
    Tx.ExprFor name iterExpr body ->
      do  iterVals <- evalExpr iterExpr >>= asList pure
          results <- forIteration name body `traverse` iterVals
          pure $ VList (location e0) results -- TODO: less listy if it's doable
    Tx.ExprCond _ i t e ->
      do  test <- evalExpr i >>= bool
          if test
            then evalExpr t
            else evalExpr e
  where
    forIteration name body value =
      scoped $
        do bindVar name value
           evalExpr body




evalSelector :: Tx.Selector -> Eval Value
evalSelector s0 =
  case s0 of
    Tx.SelName name -> getVarVal name
    Tx.SelMem s name ->
      do  subSelVal <- evalSelector s
          vals <- asList pure subSelVal
          let mems = vals >>= mem (unloc name)
          pure $ valuesToVList s0 mems
  where
    mem name v =
      case v of
        VBlock b ->
          case Map.lookup name (Value.blockValues b) of
            Nothing -> []
            Just v' -> [v']
        VTag t | unloc (Value.tagTag t) == name ->
          case Value.tagValue t of
            Nothing -> []
            Just v'  -> [v']
        VList _ l -> l >>= mem name
        _ -> []


evalCall :: Located Tx.Call -> Eval Value
evalCall lcall =
  do  args <- evalExpr `traverse` argExprs
      case name of
        Tx.FHCat ->
          do  docs <- showValue `traverse` args
              pure $ VDoc (location lcall) (PP.hcat docs)
        Tx.FVCat ->
          do  docs <- showValue `traverse` args
              pure $ VDoc (location lcall) (PP.vcat docs)
        Tx.FJoin ->
          do  (sep, vals) <- args2 args
              sep' <- showValue sep
              vals' <- list showValue vals
              pure $ VDoc (location lcall) (PP.hcat $ PP.punctuate sep' vals')
        Tx.FVJoin ->
          do  vals <- args1 args
              vals' <- list showValue vals
              pure $ VDoc (location lcall) (PP.vcat vals')
        Tx.FMkSeq -> pure $ VList (location lcall) args
        Tx.FFile ->
          do  fstr <- args1 args >>= showAsString
              case Text.lines fstr of
                [] -> throw lcall "Tranform error: cannot create file with empty name"
                [fn] -> pure (VFile (location lcall) (Text.unpack fn))
                line1:line2:_ ->
                  throw lcall
                    ("Transform error: refusing to create file with multiple line name "
                     <> q (line1 <> "\n" <> line2 <> "\n" <> "..."))
        Tx.FIsEmpty ->
          do  elts <- args1 args >>= asList pure
              case elts of
                [] -> pure $ VBool (location lcall) True
                _ -> pure $ VBool (location lcall) False
        Tx.FNot ->
          do  b <- args1 args >>= bool
              pure $ VBool (location lcall) (not b)

  where
    Tx.Call name largExprs = unloc lcall
    argExprs = unloc largExprs
    args1 args =
      case args of
        [v1] -> pure v1
        _    -> throw largExprs "Expecting this function to take 1 argument"

    args2 args =
      case args of
        [v1, v2] -> pure (v1, v2)
        _ -> throw largExprs "Expecting this function to take 2 arguments"

-------------------------------------------------------------------------------
-- "typing"

-- | Describe the type of a value. Useful for debugging/testing
describeValueType :: Value -> Text
describeValueType v0 =
  case v0 of
    VInt {} -> "integer"
    VDouble {} -> "decimal"
    VList {} -> "list"
    VString {} -> "string"
    VDoc {} -> "doc"
    VBlock b ->
      case Value.blockSchema b of
        Just a -> "block " <> q a
        Nothing -> "block"
    VTag c ->
      case Value.tagSchema c of
        Just a -> "constructor " <> q (unloc $ Value.tagTag c) <> " for union " <> q a
        Nothing -> "constructor " <> q (unloc $ Value.tagTag c)
    VFile {} -> "file"
    VBool {} -> "boolean"

list :: (Value -> Eval a) -> Value -> Eval [a]
list f v =
  case v of
    VList _ l -> f `traverse` l
    _         -> throw v "Expecting a list here"

asList :: (Value -> Eval a) -> Value -> Eval [a]
asList f v =
  case v of
    VList _ l -> f `traverse` l
    _         -> f `traverse` [v]

envValue :: Value -> Eval (Map Text Value)
envValue v =
  case v of
    VBlock b -> pure $ Value.blockValues b
    _ -> throw v "Expecting some kind of block or constructor here"

showAsString :: Value -> Eval Text
showAsString s = Text.pack . show <$> showValue s

file :: Value -> Eval FilePath
file v =
  case v of
    VFile _ f -> pure f
    _         -> throw v "Expecting a file handle here"

bool :: Value -> Eval Bool
bool v =
  case v of
    VBool _ b -> pure b
    _         -> throw v "Expecting a boolean expression here"

-------------------------------------------------------------------------------
-- misc

q :: Text -> Text
q v = "'" <> v <> "'"


valuesToVList :: HasLocation a => a -> [Value] -> Value
valuesToVList l vs =
  case vs of
    [a] -> a
    _ -> VList (location l) vs


-------------------------------------------------------------------------------
-- API

-- | Apply a transformer to validated data, returning a map from output files
-- to the output that should be written
runTransform :: Tx.Transform -> Map Ident Value -> Either Text (Map FilePath Doc)
runTransform tx blokenv =
  do
      State.evalState (Except.runExceptT run) initialEnv
  where

    run = evalDecl `traverse_` Tx.transformDecls tx >> State.gets envOutputs
    initialEnv =
      InterpEnv { envBindings = Map.empty
                , envBlockEnv = blokenv
                , envOutputs = Map.empty
                }
