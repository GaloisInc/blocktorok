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

import           Control.Applicative         ((<|>))
import qualified Control.Monad.Except        as Except
import qualified Control.Monad.State         as State

import           Data.Foldable               (traverse_)
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Prettyprinter               as PP

import           Language.Common             (HasLocation (..), Located (..),
                                              msgWithLoc, ppRange, unloc)
import           Language.Common.Units.Units (Unit)
import qualified Language.Transform.Syntax   as Tx
import           Language.Transform.Value    (Value (..), convert)
import qualified Language.Transform.Value    as Value


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

modifyVar :: Located Ident -> (Value -> Eval Value) -> Eval ()
modifyVar i f =
  do  bindVal <- State.gets (Map.lookup (unloc i) . envBindings)
      envVal <- State.gets (Map.lookup (unloc i) . envBlockEnv)
      case (bindVal, envVal) of
        (Just a, _) ->
          do  a' <- f a
              State.modify $ \s -> s { envBindings = Map.insert (unloc i) a' (envBindings s) }
        (_, Just b) ->
          do  b' <- f b
              State.modify $ \s -> s { envBlockEnv = Map.insert (unloc i) b' (envBlockEnv s) }
        _ -> throw i (q (unloc i) <> " is not defined here")

modifyEnv :: (Value -> Eval Value) -> Eval ()
modifyEnv f =
  do  bindVals <- State.gets envBindings
      envVals <- State.gets envBlockEnv

      envVals' <- f `traverse` envVals
      bindVals' <- f `traverse` bindVals

      State.modify (\s -> s { envBindings = bindVals', envBlockEnv = envVals' })


appendToFile :: FilePath -> Doc -> Eval ()
appendToFile fp doc =
  do  out <- State.gets envOutputs
      let doc' =
            case Map.lookup fp out of
              Nothing -> doc
              Just d  -> PP.vcat [d, doc]
      State.modify $ \s -> s { envOutputs = Map.insert fp doc' (envOutputs s) }


-- modTraverseBlockEnv :: (Value -> Eval Value) -> Eval ()
-- modTraverseBlockEnv f  =
--   do  env <- State.gets envBlockEnv
--       env' <- f `traverse` env
--       State.modify' (\s -> s { envBlockEnv = env'})

withBlockEnv :: Map Ident Value -> Eval a -> Eval a
withBlockEnv env' eval =
  do  env <- State.gets envBlockEnv
      State.modify (\s -> s { envBlockEnv = env'})
      result <- eval
      State.modify (\s -> s { envBlockEnv = env})
      pure result

valEnv :: Value -> Map Ident Value
valEnv v =
  case v of
    VBlock b -> Value.blockValues b
    VUnion u -> Map.singleton (unloc $ Value.tagTag (Value.unionTag u)) (Value.unionTagValue u)
    VTag t   -> maybe Map.empty valEnv (Value.tagValue t)
    _        -> Map.singleton "value" v

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
    VDouble _ d _ -> pure $ PP.pretty d
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
            Nothing         -> showEnv Map.empty renderer
            Just (VBlock b) -> showEnv (Value.blockValues b) renderer
            Just v          -> showEnv (Map.singleton "value" v) renderer
    VBlock b ->
      case Value.blockRenderer b of
        Nothing -> throw v0 ("No renderer for block at " <> ppRange (Value.blockLoc b))
        Just r -> showEnv (Value.blockValues b) r
    VUnion u -> showValue (Value.unionTagValue u)
  where
    showEnv env e = withBlockEnv env (evalExpr e) >>= showValue
evalDecl :: Tx.Decl -> Eval ()
evalDecl d0 =
  case d0 of
    Tx.DeclLet i e ->
      do  v <- evalExpr e
          bindVar i v
    Tx.DeclRender s e -> evalRender s e
    Tx.DeclFileOut f e ->
      do  f' <- getVarVal f >>= file
          doc <- evalExpr e >>= showValue
          appendToFile f' doc
    Tx.DeclIn _ sel decls ->
      do  envs <- evalSelector sel >>= asList envValue
          evalIn decls `traverse_` envs
    Tx.DeclRequire expr text ->
      do  v <- evalExpr expr >>= bool
          if not v then throw expr (unloc text)
                   else pure ()
  where
    evalIn decls env =
      scoped $ withBlockEnv env (evalDecl `traverse_` decls)


modifySelected :: (Value -> Eval Value) -> Tx.Selector -> Eval ()
modifySelected f sel = go (Tx.selectorElements sel)
  where
    go (s0:|ss) =
      case s0 of
        Tx.SelCond _ -> throw s0 "Cannot begin selector here with conditional"
        Tx.SelName name -> modifyVar name (modify ss)
        Tx.SelSchema sch -> modifyEnv (schema (unloc sch) (modify ss))

    modify ss v =
      case ss of
        []                  -> f v
        Tx.SelName name:r   -> mem (unloc name) (modify r) v
        Tx.SelSchema name:r -> schema (unloc name) (modify r) v
        Tx.SelCond expr:r   -> cond expr (modify r) v

    schema :: Ident -> (Value -> Eval Value) -> Value -> Eval Value
    schema sch g v =
      case v of
        Value.VBlock b ->
          do  values' <- schema sch g `traverse` Value.blockValues b
              let b' = b { Value.blockValues = values' }
              if Value.blockSchema b' == Just sch
                then g (VBlock b')
                else pure (VBlock b')

        Value.VUnion u ->
          do  tagv' <- schema sch g (Value.unionTagValue u) >>= tag
              let u' = u { Value.unionTag = tagv' }
              if Value.unionSchema u == Just sch
                  then g (VUnion u')
                  else pure (VUnion u')

        Value.VTag t ->
          do  arg' <- schema sch g `traverse` Value.tagValue t
              let t' = t { Value.tagValue = arg' }
              pure $ VTag t'

        VList loc l -> VList loc <$> (schema sch g `traverse` l)
        _ -> pure v


    cond :: Tx.Expr -> (Value -> Eval Value) -> Value -> Eval Value
    cond expr g v =
      do  i <- withBlockEnv (valEnv v) (evalExpr expr)
          case i of
            VBool _ True -> g v
            VBool _ False -> pure v
            VList l elts -> VList l <$> (cond expr g `traverse` elts)
            _ -> throw expr "Expecting selector expression to return a bool"

    mem :: Ident -> (Value -> Eval Value) -> Value -> Eval Value
    mem name g v =

      case v of
        Value.VBlock b ->
          case Map.lookup name (Value.blockValues b) of
            Nothing -> pure v
            Just v' ->
              do  v'' <- g v'
                  let b' = b { Value.blockValues = Map.insert name v'' (Value.blockValues b)}
                  pure (VBlock b')

        Value.VUnion u | unloc (Value.unionTagTag u) == name ->
          do  v' <- g (Value.unionTagValue u) >>= tag
              let u' = u { Value.unionTag = v' }
              pure (VUnion u')

        Value.VTag t ->
          do  v' <- mem name g `traverse` Value.tagValue t
              pure (VTag t { Value.tagValue  = v'})

        Value.VList loc vs -> VList loc <$> (mem name g `traverse` vs)

        _ -> pure v

evalRender :: Tx.Selector -> Tx.Expr -> Eval ()
evalRender sel e = modifySelected render sel
  where
    render :: Value -> Eval Value
    render vr =
      -- Trace.traceM ("\n\n>>> VALUE   :" ++ show vr) >>
      -- Trace.traceM ("\n>>> SELECTOR:" ++ show sel) >>
      -- Trace.traceM "\n\n" >>

      case vr of
        VBlock vb  -> pure $ VBlock vb { Value.blockRenderer = Just e}
        VTag vc    -> pure $ VTag vc { Value.tagRenderer = Just e }
        VList l vs -> VList l <$> render `traverse` vs
        _          -> pure vr

evalExpr :: Tx.Expr -> Eval Value
evalExpr e0 =
  case e0 of
    Tx.ExprFn lc -> evalCall lc
    Tx.ExprSelector s -> evalSelector s
    Tx.ExprLit l ->
      pure $
        case l of
          Tx.LitFloat f  -> VDouble (location f) (unloc f) Nothing
          Tx.LitInt i    -> VInt (location i) (unloc i)
          Tx.LitString s -> VString (location s) (unloc s)
    Tx.ExprFor name iterExpr body ->
      do  iterVals <- evalExpr iterExpr >>= asList pure
          results <- forIteration name body `traverse` iterVals
          pure $ VList (location e0) results -- TODO: less listy if it's doable
    Tx.ExprCond _ thens els -> cond thens els
    Tx.ExprConvertUnits e u ->
      -- TODO: every selector seems to produce a list, seems questionable
      do  quantities <- evalExpr e >>= asList quantity
          let conv (n, u') = convert throw e n u u'
          valuesToVList e <$> (conv `traverse` quantities)
  where
    forIteration name body value =
      scoped $
        do bindVar name value
           evalExpr body

    cond thens els =
      case thens of
        (i, e):rest ->
          do  test <- evalExpr i >>= bool
              if test
                then evalExpr e
                else cond rest els
        [] -> evalExpr els




evalSelector :: Tx.Selector -> Eval Value
evalSelector selector = go (Tx.selectorElements selector)
  where
    go (s0:|ss) =
      do  i <- initial s0
          vs <- select ss i
          pure $ valuesToVList selector vs

    -- TODO: clean up
    select :: [Tx.SelectorElement] -> Value -> Eval [Value]
    select ss v =
      case ss of
        [] -> pure [v]
        Tx.SelName name:r ->
          concat <$> select r `traverse` mem (unloc name) v
        Tx.SelSchema name:r ->  -- pure (schema (unloc name)) v >>= select r
          concat <$> select r `traverse` schema (unloc name) v
        Tx.SelCond expr:r ->
          do  isSatisfied <- cond expr v
              if isSatisfied
                then select r v
                else pure []

    initial s0 =
      case s0 of
        Tx.SelName name -> getVarVal name
        Tx.SelSchema _  -> throw s0 "Cannot use schema selector here"
        Tx.SelCond _    -> throw s0 "Selector cannot begin with a condition"

    schema name v =
      case v of
        VBlock b ->
          let svals = Map.toList (Value.blockValues b) >>= schema name . snd
          in if Value.blockSchema b == Just name
              then v:svals
              else svals
        VUnion u ->
          let svals = schema name (Value.unionTagValue u)
          in if Value.unionSchema u == Just name
              then v:svals
              else svals
        VTag t ->  maybe [] (schema name) (Value.tagValue t)
        VList _ elts -> elts >>= schema name
        _ -> []

    mem name v =
      case v of
        VBlock b ->
          case Map.lookup name (Value.blockValues b) of
            Nothing -> []
            Just v' -> [v']
        VUnion u | unloc (Value.tagTag (Value.unionTag u)) == name -> [Value.unionTagValue u]
        VTag t ->
          case Value.tagValue t of
            Nothing -> []
            Just v' -> [v'] >>= mem name
        VList _ l -> l >>= mem name
        _ -> []

    cond expr v =
      do  result <- withBlockEnv (valEnv v) (evalExpr expr)
          case result of
            VBool _ b -> pure b
            _ -> throw expr "Transform error: expression does not have boolean result"


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
              vals' <- asList showValue vals
              pure $ VDoc (location lcall) (PP.hcat $ PP.punctuate sep' vals')
        Tx.FVJoin ->
          do  vals <- args1 args
              vals' <- asList showValue vals
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
                _  -> pure $ VBool (location lcall) False
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
        Just a  -> "block " <> q a
        Nothing -> "block"
    VUnion u ->
      case Value.unionSchema u of
        Nothing -> "union" <> describeValueType (Value.unionTagValue u)
        Just schema -> "union " <> schema <> describeValueType (Value.unionTagValue u)
    VTag c ->
        "constructor " <> q (unloc $ Value.tagTag c)
    VFile {} -> "file"
    VBool {} -> "boolean"

-- list :: (Value -> Eval a) -> Value -> Eval [a]
-- list f v =
--   case v of
--     VList _ l -> f `traverse` l
--     _         -> throw v "Expecting a list here"

asList :: (Value -> Eval a) -> Value -> Eval [a]
asList f v =
  case v of
    VList _ l -> f `traverse` l
    _         -> f `traverse` [v]

envValue :: Value -> Eval (Map Text Value)
envValue v =
  case v of
    VBlock b -> pure $ Value.blockValues b
    _        -> throw v "Expecting some kind of block or constructor here"

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

tag :: Value -> Eval Value.TagValue
tag t =
  case t of
    VTag tv -> pure tv
    _       -> throw t "Expecting a union tag here"

quantity :: Value -> Eval (Located Double, Located Unit)
quantity v =
  case v of
    VDouble sr n (Just u) -> pure (Located sr n, u)
    _             -> throw v ("Expecting a float with units here (got " <> q (describeValueType v) <> ")")


-------------------------------------------------------------------------------
-- misc

q :: Text -> Text
q v = "'" <> v <> "'"


valuesToVList :: HasLocation a => a -> [Value] -> Value
valuesToVList l vs =
  case vs of
    [a] -> a
    _   -> VList (location l) vs

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
