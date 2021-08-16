{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.Transform.Evaluator where

import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Prettyprinter as PP
import Control.Monad(when)
import Data.Foldable(traverse_)
import Control.Applicative((<|>))

import Language.Common(Located(..), withSameLocAs, locUnknown, ppRange)
import qualified Language.Transform.Syntax as Tx
import qualified Language.Schema.Env as SchemaEnv
import qualified Language.Schema.Syntax as Schema
import qualified Language.Schema.Type as SchemaType
import qualified Language.Link.Blocktorok.Syntax as Blok

import qualified Debug.Trace as Trace



-------------------------------------------------------------------------------
-- Types
type Ident = Text
type Doc = PP.Doc ()

type Eval a = State.StateT EvalState (Except.Except Error) a

data EvalState = EvalState
  { evalStateSchema :: SchemaEnv.Env
  , evalStateValueEnv :: Value
  , evalStateBinds :: Map Ident Value
  , evalOutput :: Map FilePath Doc
  }

-------------------------------------------------------------------------------
-- Monad API

data Error =
    ErrLocated (Located Text)
  | ErrLocUnknown Text

showErr :: Error -> Text
showErr err =
  case err of
    ErrLocated l -> ppRange (locRange l) <> " " <>  locValue l
    ErrLocUnknown u -> u

runEval :: SchemaEnv.Env -> [Blok.BlockElement] -> Eval a -> Either Error a
runEval env blok ev =
    Except.runExcept $ State.evalStateT (loadRoot blok >> ev) initialState

  where
    initialState =
      EvalState { evalStateSchema = env
                , evalStateValueEnv = undefined -- TODO: kind of a hack, but should never be read
                , evalStateBinds = Map.empty
                , evalOutput = Map.empty
                }



throw :: Located a -> Text -> Eval b
throw why err = Except.throwError $ ErrLocated (err `withSameLocAs` why)

throwUnloc :: Text -> Eval b
throwUnloc err = Except.throwError $ ErrLocUnknown err

withErrorAt :: Eval a -> Located b -> Eval a
withErrorAt e wher =
  e `Except.catchError`
    \err ->
      case err of
        ErrLocUnknown msg -> throw wher msg
        ErrLocated _ -> Except.throwError err


getSchemaDef :: Located a -> Ident -> Eval Schema.SchemaDef
getSchemaDef why name =
  do  defs <- State.gets (SchemaEnv.envTypeDefs . evalStateSchema)
      case Map.lookup name defs of
        Nothing -> validationBug why ("Could not find referenced type '" <> name <> "'")
        Just a -> pure a

getBlockS :: Located a -> Ident -> Eval Schema.BlockS
getBlockS why name =
  do  def <- getSchemaDef why name
      case def of
        Schema.BlockDef blockS -> pure blockS
        Schema.UnionDef _ -> validationBug why ("Expecting '" <> name <> "' to be a block def")

getUnionS :: Located a -> Ident -> Eval Schema.Union
getUnionS why name =
  do  def <- getSchemaDef why name
      case def of
        Schema.BlockDef _ -> validationBug why ("Expecting '" <> name <> "' to be a schema def")
        Schema.UnionDef ud -> pure ud

getSchemaEnv :: Eval SchemaEnv.Env
getSchemaEnv = State.gets evalStateSchema

getValueEnv :: Eval Value
getValueEnv = State.gets evalStateValueEnv

setValueEnv :: Value -> Eval ()
setValueEnv env' = State.modify (\s -> s { evalStateValueEnv = env' })

modifyValueEnv :: (Value -> Value) -> Eval ()
modifyValueEnv f =
  State.modify (\s -> s { evalStateValueEnv = f $ evalStateValueEnv s })

withValueEnv :: Value -> Eval a -> Eval a
withValueEnv v' c =
  do  v0 <- getValueEnv
      setValueEnv v'
      a <- c
      setValueEnv v0
      pure a


loadRoot :: [Blok.BlockElement] -> Eval ()
loadRoot elts =
  do  env <- getSchemaEnv
      root <- blockEltValueMap (SchemaEnv.envRootTypes env) elts
      setValueEnv (VBlock Nothing root)

getVar :: Located Ident -> Eval (Maybe Value)
getVar name =
  do  env <- State.gets evalStateValueEnv
      let blockVal =
            case env of
              VBlock _ mp -> Map.lookup (unloc name) mp
              _ -> Nothing

      binds <- State.gets evalStateBinds
      let bindsVal = Map.lookup (unloc name) binds
      pure (blockVal <|> bindsVal)

bindVar :: Located Ident -> Value -> Eval ()
bindVar i v =
  do  var <- getVar i
      case var of
        Nothing -> State.modify (\s -> s { evalStateBinds = Map.insert (unloc i) v (evalStateBinds s)})
        Just _ -> throw i ("'" <> unloc i <> "' is already bound here")

validationBug :: Located a -> Text -> Eval b
validationBug why err = throw why ("[BUG] Vaidation bug: " <> err)

notImplemented :: Located a -> Text -> Eval b
notImplemented why err = throw why ("Not implemented: " <> err)

-- TODO: check if file is open already
-- TODO: do we allow aliasing?
openFile :: FilePath -> Eval ()
openFile path = pure ()
  -- State.modify (\s -> s { evalOutput = Map.insert path PP.emptyDoc (evalOutput s) })

appendToFile :: Located FilePath -> Doc -> Eval ()
appendToFile handle stuff =
  do  file <- State.gets (Map.lookup (unloc handle) . evalOutput)
      case file of
        Just contents ->
          let contents' = PP.vcat [contents, stuff]
          in State.modify (\s -> s {evalOutput = Map.insert (unloc handle) contents' (evalOutput s)})
        Nothing ->
          State.modify (\s -> s {evalOutput = Map.insert (unloc handle) stuff (evalOutput s)})

-------------------------------------------------------------------------------
-- Values

data Value =
    VString Text
  | VInt Integer
  | VDouble Double
  | VDoc Doc
  | VList [Value]
  | VBlock (Maybe Ident) (Map Ident Value)
  | VTagged Ident Ident Value
  | VRendered Tx.Expr Value
  | VFile FilePath
  deriving Show

describeValueType :: Blok.Value -> Text
describeValueType v =
  case v of
    Blok.Number _ -> "float"
    Blok.Ident _ -> "ident"
    Blok.String _ -> "string"
    Blok.List _ -> "list"
    Blok.Construct _ -> "variant constructor"

blockValueToValue :: SchemaType.SType -> Blok.Value -> Eval Value
blockValueToValue ty bv =
  case (bv, ty) of
    (Blok.Number n, SchemaType.SFloat) -> pure $ VDouble (unloc n)
    (Blok.Number n, SchemaType.SInt) -> pure $ VInt $ floor (unloc n)
    (Blok.Ident i, SchemaType.SIdent) -> pure $ VString (unloc i)
    (Blok.List l, SchemaType.SList eltTy) ->
      VList <$> (blockValueToValue eltTy `traverse` unloc l)
    (Blok.String s, SchemaType.SString) -> pure $ VString (unloc s)
    (Blok.Construct c, SchemaType.SNamed typeName) ->
      do  unionS <- getUnionS c typeName
          let cns = unloc c
              cname = Blok.constructorName cns
          v <- case Map.lookup (unloc cname) (Schema.unionVariants unionS) of
                      Nothing ->
                        validationBug cname
                          ("Union '" <> unloc (Schema.unionName unionS) <>
                           "' does not have " <> "constructor '" <> unloc cname <> "'")
                      Just a -> pure a
          let fieldTys = Schema.variantFields v
              fieldVs = Map.fromList $ unlocFst <$> Blok.constructorFields cns

          -- TODO: error messages could be better here
          when (Map.size fieldVs < length (Blok.constructorFields cns))
               $ throw c "Constructor is invalid - has duplicate fields"
          when (Map.keysSet fieldTys /= Map.keysSet fieldVs)
               $ throw c "Constructor is invalid - is missing or has excess fields"

          fieldTyVals <-
                case lookupPair fieldTys fieldVs `traverse` Map.keys fieldTys of
                  Nothing -> throw c "Constructor is invalid"
                  Just kvs -> pure (Map.keys fieldTys `zip` kvs)

          valMap <- Map.unions <$> (computeCnsVal `traverse` fieldTyVals)
          pure $ VTagged typeName (unloc cname) (VBlock Nothing valMap)

    _ -> throw (Blok.locateValue bv)
               ("Type error - expected '" <> Text.pack (show ty) <> "' but value here " <>
                "is a '" <> describeValueType bv <> "'")

  where
    unlocFst (a,b) = (unloc a, b)
    lookupPair m1 m2 k =
      (,) <$> Map.lookup k m1 <*> Map.lookup k m2
    computeCnsVal (i, (ty', val)) =
      Map.singleton i <$> blockValueToValue ty' val


blockElementName :: Blok.BlockElement -> Ident
blockElementName b =
  case b of
    Blok.BlockSub blk -> unloc $ Blok.blockTypeName (unloc blk)
    Blok.BlockValue i _ -> unloc i

globbedStype :: SchemaType.Globbed Schema.BlockDecl -> SchemaType.Globbed SchemaType.SType
globbedStype bd = getStype <$> bd
  where
    getStype = unloc . Schema.declType . Schema.blockDeclDecl

declStype :: SchemaType.Globbed Schema.BlockDecl -> SchemaType.SType
declStype = SchemaType.unGlob . globbedStype

blockEltLocatedName :: Blok.BlockElement -> Located Ident
blockEltLocatedName be =
  case be of
    Blok.BlockSub blok -> Blok.blockTypeName (unloc blok)
    Blok.BlockValue i _ -> i

blockEltToValue :: SchemaType.Globbed Schema.BlockDecl -> Blok.BlockElement -> Eval Value
blockEltToValue decl be =
  case (be, declStype decl) of
    (Blok.BlockSub sub, SchemaType.SNamed typeName) ->
      do  blockS' <- getBlockS sub typeName
          mkBlockValue blockS' sub

    (Blok.BlockValue _ bv, s) -> blockValueToValue s bv

    _ -> validationBug (blockEltLocatedName be) "block element does not match schema"

blockEltValueMap ::
  Map Ident (SchemaType.Globbed Schema.BlockDecl) ->
  [Blok.BlockElement] ->
  Eval (Map Ident Value)
blockEltValueMap smap elts = Map.unions <$> (eltMap `traverse` Map.toList smap)
  where
    eltsFor name = filter ((==name) . blockElementName) elts
    eltMap (name, decl) =
      do  vals <- blockEltToValue decl `traverse` eltsFor name
          case (vals, () <$ globbedStype decl) of
            ([v], SchemaType.One {}) ->
              pure $ Map.singleton name v

            ([], SchemaType.Optional {}) ->
              pure Map.empty

            ([v], SchemaType.Optional {}) ->
              pure $ Map.singleton name v

            (vs@(_:_), SchemaType.Some {}) ->
              pure $ Map.singleton name (VList vs)

            (vs, SchemaType.Many {}) ->
              pure $ Map.singleton name (VList vs)

            -- in some cases (the root block) there is nowhere to point the error
            _ ->
              let why =
                    case eltsFor name of
                      e:_ -> () <$ blockEltLocatedName e
                      []  -> locUnknown ()
              in validationBug why ("number of instances of '" <> name <> "' does not match glob pattern")

mkBlockValue :: Schema.BlockS -> Located Blok.Block -> Eval Value
mkBlockValue blockS locBlock =
    VBlock (Just . unloc $ Schema.blockSType blockS) <$> fields
  where
    block = unloc locBlock
    nameField =
      case (Schema.blockSName blockS, Blok.blockName block) of
        (Nothing, Nothing) -> pure Map.empty
        (Just k, Just v)   -> pure $ Map.singleton (unloc k) (VString $ unloc v)
        (Just _, Nothing)  -> validationBug locBlock "mkBlock - schema has name / block doesn't"
        (Nothing, Just _)  -> validationBug locBlock "mkBlock - schema doesn't have name / block does"

    fields =
      do  nf <- nameField
          fs <- blockEltValueMap (Schema.blockSFields blockS) (Blok.blockContents block)
          pure $ Map.union nf fs

transformValue :: (Value -> Eval Value) -> Value -> Eval Value
transformValue f v0 = v' >>= f
  where
    v' =
      case v0 of
        VString _ -> pure v0
        VInt _ -> pure v0
        VDouble _ -> pure v0
        VDoc _ -> pure v0
        VList vs -> VList <$> (transformValue f `traverse` vs)
        VBlock i m -> VBlock i <$> (transformValue f `traverse` m)
        VTagged ty tag v -> VTagged ty tag <$> transformValue f v
        VRendered d v -> VRendered d <$> transformValue f v
        VFile _ -> pure v0

renderSelectedPath :: [Ident] -> Tx.Expr -> Value -> Value
renderSelectedPath path e v =
  case path of
    [] ->
      case v of
        VRendered _ v' -> VRendered e v'
        VList vs -> VList (renderSelectedPath path e <$> vs)
        _ -> VRendered e v
    n:r ->
      case v of
        VBlock mbN mp ->
          case Map.lookup n mp of
            Nothing -> v
            Just v' ->
              VBlock mbN $ Map.insert n (renderSelectedPath r e v') mp
        VTagged t n' v'  | n == n' ->
            VTagged t n' $ renderSelectedPath r e v'
        VList vs -> VList (renderSelectedPath path e <$> vs)
        _ -> v

renderValueSelector :: Tx.Selector -> Tx.Expr -> Value -> Value
renderValueSelector sel e v0 =
  case selectorToPath sel of
    []  -> v0 -- TODO: this is impossible
    schema:path ->
      case v0 of
        VBlock ty _ | ty == Just (unloc schema) ->
          renderSelectedPath (identPath path) e v0
        VTagged ty _ _| ty == unloc schema ->
          renderSelectedPath (identPath path) e v0
        VList vs -> VList (renderValueSelector sel e <$> vs)
        VRendered r vr ->
          case path of
            [] -> renderValueSelector sel e vr
            _ ->  VRendered r (renderValueSelector sel e vr)
        _ -> v0
  where
    identPath path = unloc <$> path


selectorToPath :: Tx.Selector -> [Located Ident]
selectorToPath sel =
  case sel of
    Tx.SelName i -> [i]
    Tx.SelMem m i -> selectorToPath m ++ [i]

-------------------------------------------------------------------------------
-- Evaluation

showValue :: Value -> Eval Doc
showValue v =
  case v of
    VString s -> pure $ PP.pretty s
    VInt i -> pure $ PP.pretty i
    VDouble d -> pure $ PP.pretty d
    VList l -> PP.hcat <$> (showValue `traverse` l)
    VRendered e vr ->
      withValueEnv vr (evalExpr e) >>= showValue
    VDoc d -> pure d
    VTagged {} -> throwUnloc "Tried to show union value (but renderer has not been defined)"
    VBlock (Just t) _ -> throwUnloc $ "Tried to show block '" <> t <>
                                      "' (but renderer has not been defined)"
    VBlock {} -> throwUnloc "Tried to show block (but renderer has not been defined)"
    VFile {} -> throwUnloc "Tried to show file"

evalDecl :: Tx.Decl -> Eval ()
evalDecl d =
  case d of
    Tx.DeclRender s e ->
      do  env <- getValueEnv
          env' <- transformVal s e env
          setValueEnv env'
    Tx.DeclFileOut handle e ->
      do  outVal' <- evalExpr (unloc e)
          outVal <- showValue outVal'
          file <- getVar handle
          case file of
            Just (VFile f) -> appendToFile (f `withSameLocAs` handle) outVal
            Nothing -> throw handle ("Symbol '"  <> unloc handle <> "' is not defined")
            _ -> throw handle ("Symbol '"  <> unloc handle <> "' is not a file")
    Tx.DeclLet n e ->
      do  v <- evalExpr (unloc e)
          bindVar n v


  where
    transformVal s e = transformValue (pure  . renderValueSelector s (unloc e))

evalLit :: Tx.Lit -> Eval Value
evalLit l =
  case l of
    Tx.LitString s -> pure $ VString (unloc s)
    Tx.LitInt i -> pure $ VInt (unloc i)
    Tx.LitFloat f -> pure $ VDouble (unloc f)

evalExpr :: Tx.Expr -> Eval Value
evalExpr e0 =
  case e0 of
    Tx.ExprFn fname args ->
      do  argvs <- evalExpr `traverse` (unloc <$> args)
          let argvs' = zipWith withSameLocAs argvs args
          evalFn fname argvs'
    Tx.ExprSelector sel -> VList <$> evalSelector sel
    Tx.ExprLit l -> evalLit l


evalFn :: Tx.FName -> [Located Value] -> Eval Value
evalFn fn args =
  case fn of
    Tx.FHCat ->
      do  args' <- showValue `traverse` uargs
          pure . VDoc $ PP.hcat args'
    Tx.FVCat ->
      do  args' <- showValue `traverse` uargs
          pure . VDoc $ PP.vcat args'
    Tx.FFile ->
      case args of
        [v] ->
          do  path <- Text.unpack <$> str v
              openFile path
              pure (VFile path)
        _ -> throwUnloc "invalid arguments to file open"
    Tx.FJoin ->
      case args of
        [sep, valArr] ->
          do  sep' <- showValue (unloc sep)
              vals' <- arr asDoc valArr
              let doc = PP.hcat (PP.punctuate sep' vals')
              pure $ VDoc doc
        _ -> throwUnloc "invalid arguments to join"
    Tx.FMkSeq -> pure (VList uargs)
  where
    uargs = unloc <$> args
    asDoc v = showValue v
    arr f v =
      case unloc v of
        VList elts -> f `traverse` elts
        _ -> throw v "expecting list here"

    str v =
      case unloc v of
        VString s -> pure s
        _ -> throw v "expecting string here"




evalSelector :: Tx.Selector -> Eval [Value]
evalSelector sel0 =
  case sel0 of
    Tx.SelName i ->
      do  val <- getVar i
          case val of
            Nothing -> pure []
            Just a -> pure [a]

    Tx.SelMem sel' m ->
      do  inner <- evalSelector sel'
          concat <$> (select m `traverse` inner)

  where
    select i v =
      case v of
        VList vs ->
          concat <$> (select i `traverse` vs)
        VRendered _ v' -> select i v'
        VBlock _ fields ->
          case Map.lookup (unloc i) fields of
            Nothing -> pure []
            Just a -> pure [a]
        _ -> validationBug i ("Cannot select '" <> unloc i <> "' from simple value")


-------------------------------------------------------------------------------
-- misc

unloc :: Located a -> a
unloc = locValue


-------------------------------------------------------------------------------
-- API

runTransform :: SchemaEnv.Env -> Tx.Transform -> [Blok.BlockElement] -> Either Error (Map FilePath Doc)
runTransform env tx belts =
  runEval env belts go
  where
    go =
      do  evalDecl `traverse_` (unloc <$> Tx.transformDecls tx)
          State.gets evalOutput
