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
  , UnionValue(..)
  , unionTagValue
  , unionTagTag
  , Value(..)
    -- ** Describing, traversing, validating values
  , describeValue
  , mapSelected
  , validateElts
  , valueToList
    -- ** Misc. utilities
  , convert
  ) where

import qualified Control.Monad.Reader        as Reader
import qualified Control.Monad.Validate      as Validate

import           Data.Foldable               (traverse_)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Prettyprinter               as PP

import qualified Language.Blocktorok.Syntax  as Blok
import           Language.Common             (HasLocation (..), Located (..),
                                              SourceRange, msgWithLoc,
                                              sourceRangeSpan', unloc,
                                              withSameLocAs)
import           Language.Common.Units.Units (Unit)
import qualified Language.Common.Units.Units as Units
import qualified Language.Schema.Env         as Schema
import qualified Language.Schema.Syntax      as Schema
import qualified Language.Schema.Type        as Schema
import qualified Language.Transform.Syntax   as Tx

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
    -- | The argument to the variant constructor
  , tagValue    :: Maybe Value
  }
  deriving(Show)

data UnionValue = UnionValue
  { unionTag    :: TagValue
  , unionSchema :: Maybe Ident
  }
  deriving(Show)

unionTagValue :: UnionValue -> Value
unionTagValue = VTag . unionTag

unionTagTag :: UnionValue -> Located Ident
unionTagTag = tagTag . unionTag

instance HasLocation TagValue where
  location = tagLoc

-- | Representation of block values, carrying their rendering rule and type
-- information
data BlockValue = BlockValue
  { -- | The source location of this block
    blockLoc      :: SourceRange
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
    VDouble SourceRange Double (Maybe (Located Unit))
  | VInt SourceRange Integer
  | VList SourceRange [Value]
  | VString SourceRange Text
  | VTag TagValue
  | VUnion UnionValue  -- "container" for union values

  | VFile SourceRange FilePath
  | VDoc SourceRange Doc
  | VBlock BlockValue
  | VBool SourceRange Bool
  deriving(Show)

instance HasLocation Value where
  location v =
    case v of
      VDouble r _ Nothing  -> r
      VDouble r _ (Just u) -> sourceRangeSpan' r u
      VInt r _             -> r
      VBool r _            -> r
      VList r _            -> r
      VString r _          -> r

      VDoc r _             -> r
      VFile r _            -> r
      VTag t               -> location t
      VBlock b             -> location b
      VUnion t             -> location (unionTagValue t)

-- traverseValue :: Monad m => (Value -> m Value) -> Value -> m Value
-- traverseValue f v =
--   case v of
--     VDouble {} -> f v
--     VInt {} -> f v
--     VString {} -> f v
--     VBool {} -> f v
--     VFile {} -> f v
--     VList r l ->
--       (traverseValue f `traverse` l) >>= f . VList r

--     VDoc {} -> f v
--     VTag t ->
--       do  v' <- traverseValue f `traverse` tagValue t
--           f (VTag t { tagValue = v'})

--     VBlock b ->
--       do  v' <- traverseValue f `traverse` blockValues b
--           f (VBlock b { blockValues = v' })

--     VUnion t ->
--       do  v' <- traverseValue f (unionTag t)
--           f (VUnion )

-- | Return a textual description of a 'Value', useful for debugging and
-- testing
describeValue :: Value -> Text
describeValue v =
  case v of
    VBlock b     ->
      case blockSchema b of
        Nothing -> "block"
        Just s  -> "block of type " <> q s
    VTag c       -> "tag " <> unloc (tagTag c)
    VString _ s  -> "string " <> showT s
    VInt _ i     -> "int " <> showT i
    VDouble _ i Nothing -> "double " <> showT i
    VDouble _ i (Just u) -> "double " <> showT i <> " in " <> showT (unloc u)
    VBool _ b    -> "boolean " <>  showT b
    VDoc _ d     -> "doc " <> Text.pack (take 50 (show d))
    VFile _ f    -> "file " <> Text.pack f
    VList _ l    -> "[" <> Text.intercalate ", " (describeValue <$> l) <> "]"
    VUnion t     -> describeValue (unionTagValue t)

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

        VTag c | unloc (tagTag c) == n ->
          case r of
            []  -> f v
            _:_ -> VTag . mkCns c <$> (mapSelected f r `traverse` tagValue c)

        VList loc vs ->
          VList loc <$> (mapSelected f path `traverse` vs)
        _ -> pure v
  where
    mkBlock b elts' = b { blockValues = Map.fromList elts' }
    mkCns c elts' = c { tagValue = elts' }

    mapElt p r (n, v') | n == p =  (n,) <$> mapSelected f r v'
                       | otherwise = pure (n, v')

-- | Traverse a 'Value', running an action at each entity described by the
-- schema named by the provided 'Ident'
-- traverseSchemaValues :: Monad m => (Value -> m Value) -> Ident -> Value -> m Value
-- traverseSchemaValues f i = traverseValue sch
--   where
--     sch v =
--       case v of
--         VBlock b | blockSchema b == Just i -> f v
--         VTag t | tagSchema t == Just i -> f v
--         VList r vs -> VList r <$> (traverseSchemaValues f i `traverse` vs)
--         _ -> pure v


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

-- | @convert thrower why n to from@ converts the value @n@ expressed in unit
-- @from@ to be expressed in unit @to@, throwing an error (via @thrower why@)
-- if the units are not of compatible dimension
convert :: (HasLocation why, Monad m)
        => (why -> Text -> m Value)
        -> why
        -> Located Double
        -> Located Unit
        -> Located Unit
        -> m Value
convert thrower why n to from
  | Units.unitDimension (unloc from) == Units.unitDimension (unloc to) =
    let fromRatio' = fromRational (Units.unitCanonicalConvRatio (unloc from))
        toRatio' = fromRational (Units.unitCanonicalConvRatio (unloc to))
        n' = unloc n * fromRatio' / toRatio'
    in pure $ VDouble (location n) n' (Just to)
  | otherwise = thrower why ("Cannot convert from " <> q (showT (unloc from)) <> " to " <>
                           q (showT (unloc to)))

validateValue :: Schema.SType -> Value -> Val Value
validateValue ty val =
  case val of
    VDouble _ _ Nothing -> req $ Schema.SFloat Nothing
    VDouble sr n (Just u) ->
      case ty of
        Schema.SFloat (Just ud) ->
          if Units.unitDimension ud == Units.unitDimension (unloc u)
            then convert throw val (n `withSameLocAs` sr) (ud `withSameLocAs` u) u
            else throw u (q (showT (unloc u)) <> " has a difference dimension than " <> q (showT ud))

        _ -> unexpected ("double in " <> showT u)
    VInt {} -> req Schema.SInt
    VString {} -> req Schema.SString
    VList {} -> unexpected "list"
    VDoc {} -> unexpected "doc"
    VFile {} -> unexpected "file"
    VBool {} -> req Schema.SBool

    VUnion vu ->
      do  let t = unionTag vu
          n <- reqNamed "union constructor"
          union <- getUnion n
          argType <- getVariantArgType union t
          let vu' = vu { unionSchema = Just $ unloc (Schema.unionName union)}

          case (tagValue t, argType) of
            (Nothing, Nothing) -> pure $ VUnion vu'
            (Just val', Just ty') ->
              do  tagVal' <- validateValue ty' val'
                  let t' = t { tagValue = Just tagVal' }
                  pure $ VUnion (vu' { unionTag = t'})
            (Nothing, _) ->
              throw val "Was expecting this tag to take an argument"
            (_, Nothing) ->
              throw val "Was not expecting this tag to take an argument"
    VTag {} -> unexpected "tag"

    VBlock block ->
      do  n <- reqNamed "block"
          blockS <- getBlock n
          let fieldTys = Schema.blockSFields blockS
          fvals' <- validateBlockLike (location block) (blockValues block) fieldTys
          pure $ VBlock (block { blockValues =  fvals', blockSchema = Just n})

  where
    -- tag stuff
    getVariantArgType union c =
      case Map.lookup (unloc $ tagTag c) (Schema.unionVariants union) of
        Nothing ->
          throw (tagTag c) ("Tag " <> q (unloc $ tagTag c) <> " is not part of union " <>
                           q (unloc $ Schema.unionName union))
        Just v -> pure (Schema.variantArg v)

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

-- TODO: should we just parse to the `Value` in this module?

blockValueToValue :: Blok.Value -> Value
blockValueToValue e =
  case e of
    Blok.Double n u -> VDouble (location n) (unloc n) u
    Blok.Int n -> VInt (location n) (unloc n)
    Blok.String s -> VString (location s) (unloc s)
    Blok.List l -> VList (location l) (blockValueToValue <$> unloc l)
    Blok.Tag i v ->
      let tag = TagValue { tagLoc = location $ Blok.locateValue e
                         , tagRenderer = Nothing
                         , tagTag = i
                         , tagValue = blockValueToValue <$> v
                         }
      in VUnion UnionValue { unionTag = tag, unionSchema = Nothing }
    Blok.Block elts ->
      VBlock $
        BlockValue  { blockLoc = location elts
                    , blockRenderer = Nothing
                    , blockSchema = Nothing
                    , blockValues = eltsToValueMap (unloc elts)
                    }

eltsToValueMap :: [Blok.BlockElement] -> Map Ident Value
eltsToValueMap elts = foldr mapWithVal Map.empty keyvals'
  where
    keyvals = blocktuple <$> elts
    keyvals' = fmap blockValueToValue <$> keyvals
    blocktuple (Blok.BlockElement key val) = (key, val)
    joinVals a b = VList (sourceRangeSpan' a b) (valueToList a ++ valueToList b)
    mapWithVal (k, v) m =
      Map.insertWith joinVals (unloc k) v m

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
    elts' = eltsToValueMap (unloc elts)
    validate = validateBlockLike (location elts) elts' (Schema.envRootTypes env)

