{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{-|
Module      : Language.Transform.Value
Description : Blocktorok transformer language values
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : james.lamar@galois.com
Stability   : experimental
Portability : N/A

Definitions of and methods for working with values in the Blocktorok transform
language. These values are the result of evaluating 'Expr's which are to be
interpolated in bar strings, and may eventually be the parse target for actual
Blocktorok data (currently, those values are translated into these values.)
-}

module Language.Transform.Value
  ( -- * Blocktorok transform values
    -- ** Values
    BlockValue(..)
  , TagValue(..)
  , Value(..)
    -- ** Describing, traversing, validating values
  , describeValue
  , mapSelected
  , traverseSchemaValues
  , validateElts
  , valueToList
  ) where

import qualified Control.Monad.Reader       as Reader
import qualified Control.Monad.Validate     as Validate

import           Data.Bifunctor             (first)
import           Data.Foldable              (traverse_)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import qualified Prettyprinter              as PP

import qualified Language.Blocktorok.Syntax as Blok
import           Language.Common            (HasLocation (..), Located (..),
                                             SourceRange, msgWithLoc, unloc)
import qualified Language.Schema.Env        as Schema
import qualified Language.Schema.Syntax     as Schema
import qualified Language.Schema.Type       as Schema
import qualified Language.Transform.Syntax  as Tx

type Doc = PP.Doc ()
type Ident = Text

-- | Representation of union variant values, carrying their rendering rule and
-- type information
data TagValue = TagValue
  { -- | The source location of this variant
    tagLoc      :: SourceRange
    -- | The variant's tag
  , tagTag      :: Located Ident
    -- | How to render this variant, if defined
  , tagRenderer :: Maybe Tx.Expr
    -- | The fields of the variant and their values
  , tagValue    :: Map Ident Value
    -- | The type of this variant, if defined
  , tagSchema   :: Maybe Ident
  }
  deriving(Show)

instance HasLocation TagValue where
  location = tagLoc

-- | Representation of block values, carrying their rendering rule and type
-- information
data BlockValue = BlockValue
  { -- | The source location of this block
    blockLoc      :: SourceRange
    -- | The 'type' of the block; its leading identifier
  , blockType     :: Located Ident
    -- | The name of the block, if required in the schema
  , blockName     :: Maybe (Located Ident)
    -- | How to render this block, if defined
  , blockRenderer :: Maybe Tx.Expr
    -- | The fields of the block and their values
  , blockValues   :: Map Ident Value
    -- | The type of this block, if defined
  , blockSchema   :: Maybe Ident
  }
  deriving(Show)

instance HasLocation BlockValue where
  location = blockLoc

-- | Values in the transformer language, which correspond closely to the values
-- defined in "Language.Blocktorok.Syntax"
data Value =
    VDouble SourceRange Double
  | VInt SourceRange Integer
  | VList SourceRange [Value]
  | VString SourceRange Text

  | VFile SourceRange FilePath
  | VDoc SourceRange Doc
  | VConstruct TagValue
  | VBlock BlockValue
  deriving(Show)

instance HasLocation Value where
  location v =
    case v of
      VDouble r _  -> r
      VInt r _     -> r
      VList r _    -> r
      VString r _  -> r

      VDoc r _     -> r
      VFile r _    -> r
      VConstruct b -> location b
      VBlock b     -> location b

traverseValue :: Monad m => (Value -> m Value) -> Value -> m Value
traverseValue f v =
  case v of
    VDouble {} -> f v
    VInt {} -> f v
    VString {} -> f v
    VFile {} -> f v
    VList r l ->
      (traverseValue f `traverse` l) >>= f . VList r

    VDoc {} -> f v
    VConstruct c ->
      do  v' <- traverseValue f `traverse` tagValue c
          f (VConstruct c { tagValue = v'})

    VBlock b ->
      do  v' <- traverseValue f `traverse` blockValues b
          f (VBlock b { blockValues = v' })

-- | Return a textual description of a 'Value', useful for debugging and
-- testing
describeValue :: Value -> Text
describeValue v =
  case v of
    VBlock b     -> "block " <> unloc (blockType b)
    VConstruct c -> "constructor " <> unloc (tagTag c)
    VString _ s  -> "string " <> showT s
    VInt _ i     -> "int " <> showT i
    VDouble _ i  -> "double " <> showT i
    VDoc _ d     -> "doc " <> Text.pack (take 50 (show d))
    VFile _ f    -> "file " <> Text.pack f
    VList _ l    -> "[" <> Text.intercalate ", " (describeValue <$> l) <> "]"

-- | Map an action over the selected parts of a 'Value'. The list of 'Ident'
-- corresponds to a path described by a 'Selector'
mapSelected :: Monad f => (Value -> f Value) -> [Ident] -> Value -> f Value
mapSelected f path v =
  case path of
    [] ->
      case v of
        VList loc vs ->
          VList loc <$> (mapSelected f path `traverse` vs)
        _ -> f v
    n:r ->
      case v of
        VBlock b ->
          VBlock . mkBlock b <$> (mapElt n r `traverse`  Map.toList (blockValues b))
        -- TODO: a bit of a hack because of VConstruct
        VConstruct c | unloc (tagTag c) == n ->
          case r of
            [] -> f v
            n':r' ->  VConstruct . mkCns c <$> (mapElt n' r' `traverse` Map.toList (tagValue c))
        VList loc vs ->
          VList loc <$> (mapSelected f path `traverse` vs)
        _ -> pure v
  where
    mkBlock b elts' = b { blockValues = Map.fromList elts' }
    mkCns c elts' = c { tagValue = Map.fromList elts' }

    mapElt p r (n, v') | n == p =  (n,) <$> mapSelected f r v'
                       | otherwise = pure (n, v')

-- | Traverse a 'Value', running an action at each entity described by the
-- schema named by the provided 'Ident'
traverseSchemaValues :: Monad m => (Value -> m Value) -> Ident -> Value -> m Value
traverseSchemaValues f i = traverseValue sch
  where
    sch v =
      case v of
        VBlock b | blockSchema b == Just i -> f v
        VConstruct t | tagSchema t == Just i -> f v
        VList r vs -> VList r <$> (traverseSchemaValues f i `traverse` vs)
        _ -> pure v


--

type Val a = Validate.ValidateT [Text] (Reader.Reader Schema.Env) a

runVal :: Schema.Env -> Val a -> Either [Text] a
runVal e v = Reader.runReader (Validate.runValidateT v) e

getSchemaDef :: HasLocation why => why -> Ident -> Val Schema.SchemaDef
getSchemaDef why name =
  do  def <- Reader.asks (Map.lookup name . Schema.envTypeDefs)
      case def of
        Nothing ->
          throw why ("[BUG] Could not find definition for schema " <> q name <> " used here")
        Just a -> pure a

--

throw :: HasLocation a => a -> Text -> Val b
throw why msg = Validate.refute [msgWithLoc why msg]


validateBlockLike :: SourceRange -> Map Ident Value ->  Map Ident (Schema.Globbed Schema.BlockDecl) -> Val (Map Ident Value)
validateBlockLike why fieldVals fieldTys =
  do  fvals' <- validateBlockField `traverse` Map.toList fieldTys
      flagSuperfluousBlockElt `traverse_` Map.toList fieldVals
      pure $ Map.fromList fvals'
  where
      -- block stuff
    validateBlockField (n, bd) =
      do  let gty = unloc . Schema.declType . Schema.blockDeclDecl <$> bd
              mbVal = Map.lookup n fieldVals
              vals = maybe [] valueToList mbVal

          vbVals' <- validateValue (Schema.unGlob gty) `traverse` vals
          let val' = (n, VList why vbVals')
          case (gty, vbVals') of
            (Schema.One {}, [_]) -> pure val'
            (Schema.One {}, _) ->
              throw why ("Expecting exactly one " <> n <> " in this block")
            (Schema.Some {}, []) ->
              throw why ("Expecting at least one " <> n <> " in this block")
            (Schema.Some {}, _) -> pure val'
            (Schema.Many {}, _) -> pure val'
            (Schema.Optional {}, []) -> pure val'
            (Schema.Optional {}, [_]) -> pure val'
            (Schema.Optional {}, _) ->
              throw why ("Expecting at most one " <> n <> " in this block")

    flagSuperfluousBlockElt (ln, _) =
      case Map.lookup ln fieldTys of
        Nothing -> throw why (q ln <> " is not part of this block")
        Just _  -> pure ()

-- TODO: Get rid of those `undefined`s for units
validateValue :: Schema.SType -> Value -> Val Value
validateValue ty val =
  case val of
    VDouble loc d ->
      case ty of
        -- TODO: handle this during parsing
        (Schema.SInt _) -> pure $ VInt loc (floor d)
        _           -> req (Schema.SFloat undefined)
    VInt {} -> req (Schema.SInt undefined)
    VString {} -> req Schema.SString
    VList {} -> unexpected "list"
    VDoc {} -> unexpected "doc"
    VFile {} -> unexpected "file"
    VConstruct cns ->
      do  n <- reqNamed "union constructor"
          union <- getUnion n
          fieldTys <- constructorFields cns union
          fvals' <- validateConstructorField cns `traverse` Map.toList fieldTys
          flagExtraFieldError cns fieldTys `traverse_` Map.toList (tagValue cns)
          pure $ VConstruct (cns { tagValue =  Map.fromList fvals', tagSchema = Just n })
    VBlock block ->
      do  n <- reqNamed "block"
          blockS <- getBlock n
          let fieldTys = Schema.blockSFields blockS
          fvals' <- validateBlockLike (location block) (blockValues block) fieldTys
          pure $ VBlock (block { blockValues =  fvals', blockSchema = Just n})
  where
    -- constructor stuff
    flagExtraFieldError why fieldTys (ln, _) =
      case Map.lookup ln fieldTys of
        Nothing -> throw why ("Field " <> ln <> " is not part of this union")
        Just _  -> pure ()

    validateConstructorField c (n,ty') =
      case Map.lookup n (tagValue c) of
        Nothing -> throw val ("Constructor " <> q (unloc $ tagTag c) <> " is missing required value " <> q n)
        Just val' ->
          do  val'' <- validateValue ty' val'
              pure (n, val'')

    constructorFields c u =
      case Map.lookup (unloc $ tagTag c) (Schema.unionVariants u) of
        Just v -> pure $ Schema.variantFields v
        Nothing ->
          throw (tagTag c) ("Constructor " <> q (unloc $ tagTag c) <> " is not part of union " <>
                            q (unloc $ Schema.unionName u))

    -- misc stuff
    getUnion n =
      do  def <- getSchemaDef val n
          case def of
            Schema.UnionDef u  -> pure u
            Schema.BlockDef {} -> unexpected "union constructor"

    getBlock n =
      do  def <- getSchemaDef val n
          case def of
            Schema.UnionDef {} -> unexpected "block"
            Schema.BlockDef b  -> pure b

    reqNamed ue =
      case ty of
        Schema.SNamed n -> pure n
        _               -> unexpected ue

    unexpected n =  throw val ("Not expecting " <> n <> " here - should be a value of type " <> q (showT ty))
    req ty' = requireType val ty ty' >> pure val

requireType :: HasLocation why => why -> Schema.SType -> Schema.SType -> Val ()
requireType why expected actual =
    if expected == actual
    then pure ()
    else throw why ("Expected " <> q (showT expected) <> " but actual type here is " <> q (showT actual))

--

-- TODO: should we just parse to the `Value` in this module?

blockValueToValue :: Blok.Value -> Val Value
blockValueToValue e =
  case e of
    Blok.Number n -> pure $ VDouble (location n) (unloc n)
    Blok.String s -> pure $ VString (location s) (unloc s)
    Blok.List l -> VList (location l)  <$> (blockValueToValue `traverse` unloc l)
    Blok.Construct cns ->
      do  let c = unloc cns
          flagDuplicates (Blok.constructorFields c) `traverse_` Blok.constructorFields c
          fs <- blockValueToValue `traverse` fieldMap c
          pure . VConstruct $
            TagValue  { tagLoc = location cns
                      , tagRenderer = Nothing
                      , tagSchema = Nothing
                      , tagTag = Blok.constructorName c
                      , tagValue = fs
                      }
  where
    fieldMap c =
      Map.fromList $ first unloc <$> Blok.constructorFields c

    flagDuplicates l (n, _) =
      case [n | (n', _) <- l, unloc n == unloc n' ] of
        [] -> throw n "[BUG] Constructor field somehow not initialized?"
        [_] -> pure ()
        _:elt:_ -> throw elt ("Constructor field " <> q (unloc n) <> " appears more than once")

eltsToValueMap :: SourceRange -> [Blok.BlockElement] -> Val (Map Ident Value)
eltsToValueMap why elts =
  do  vals <- concat <$> eltVal `traverse` elts
      flagDuplicates vals `traverse_` vals
      subs <- Map.unionsWith joinList <$> subBlocks `traverse` elts
      flagBlockValDuplicates (Map.toList subs) `traverse_` vals
      let vals' = Map.fromList (first unloc <$> vals)

      pure $ Map.union vals' subs
  where
    joinList v1 v2 =
      case (v1, v2) of
        (VList r vs1, VList _ vs2) -> VList r (vs1 ++ vs2)
        _ -> error "[BUG] expecting all elements of block union to be lists"


    subBlocks elt =
      case elt of
        Blok.BlockSub block' ->
          do  bv' <- blockBlockToValue block'
              pure $ Map.singleton (unloc $ Blok.blockTypeName (unloc block')) (VList (location why) [bv'])
        Blok.BlockValue {} ->
          pure Map.empty

    eltVal elt =
      case elt of
        Blok.BlockSub _ -> pure []
        Blok.BlockValue name v ->
          do  v' <- blockValueToValue v
              pure [(name, v')]

    flagBlockValDuplicates l (n, _) =
      case [n | (n', _) <- l, unloc n == n' ] of
        [] -> pure ()
        elt:_ -> throw elt ("Block value and sub block have same name " <> q (unloc n))

    flagDuplicates l (n, _) =
      case [n | (n', _) <- l, unloc n == unloc n' ] of
        [] -> throw n "[BUG] flagDuplicates: Block field somehow not initialized?"
        [_] -> pure ()
        _:elt:_ -> throw elt ("Block value field " <> q (unloc n) <> " appears more than once")


blockBlockToValue :: Located Blok.Block -> Val Value
blockBlockToValue block =
  do  let b = unloc block
      vmap <- eltsToValueMap (location block) (Blok.blockContents b)
      pure $
        VBlock
          BlockValue { blockLoc = location block
                     , blockRenderer = Nothing
                     , blockSchema = Nothing
                     , blockType = Blok.blockTypeName b
                     , blockName = Blok.blockName b
                     , blockValues = vmap
                     }

--

-- | @valueToList v@ returns the underlying list if @v@ is a @VList@, and
-- injects @v@ into a singleton list otherwise
valueToList :: Value -> [Value]
valueToList v0 =
  case v0 of
    VList _ vs -> vs
    _          -> [v0]

showT :: Show a => a -> Text
showT = Text.pack . show

q :: Text -> Text
q a = "'" <> a <> "'"



-------------------------------------------------------------------------------
--

-- | Given a schema and collection of 'BlockElement's, validate that the
-- elements satisfy the schema, returning a value environment upon success
validateElts :: Schema.Env -> Located [Blok.BlockElement] -> Either Text (Map Ident Value)
validateElts env elts =
  case runVal env validate of
    Left errs -> Left (Text.unlines errs)
    Right a   -> pure a

  where
    validate =
      do  elts' <- eltsToValueMap (location elts) (unloc elts)
          validateBlockLike (location elts) elts' (Schema.envRootTypes env)


