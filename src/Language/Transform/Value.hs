{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.Transform.Value where


import qualified Prettyprinter as PP
import Data.Text(Text)
import qualified Data.Text as Text
import Language.Common(SourceRange, Located(..), HasLocation(..), msgWithLoc, unloc)
import qualified Data.Map as Map
import Data.Foldable(traverse_)
import qualified Language.Link.Blocktorok.Syntax as Blok

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Validate as Validate

import qualified Language.Transform.Syntax as Tx
import qualified Language.Schema.Syntax as Schema
import qualified Language.Schema.Env as Schema
import qualified Language.Schema.Type as Schema

type Doc = PP.Doc ()
type Ident = Text

data TagValue = TagValue
  { tagTag :: Located Ident
  , tagRenderer :: Maybe Tx.Expr
  , tagValue :: [(Located Ident, Value)]
  , tagSchema :: Maybe Ident
  }

data BlockValue = BlockValue
  { blockType :: Located Ident
  , blockName :: Maybe (Located Ident)
  , blockRenderer :: Maybe Tx.Expr
  , blockValues :: [(Located Ident, Value)]
  , blockSchema :: Maybe Ident
  }

data Value =
    VDouble SourceRange Double
  | VInt SourceRange Integer
  | VIdent SourceRange Text
  | VList SourceRange [Value]
  | VString SourceRange Text

  | VDoc SourceRange Doc
  | VConstruct SourceRange TagValue
  | VBlock SourceRange BlockValue

instance HasLocation Value where
  location v =
    case v of
      VDouble r _ -> r
      VInt r _ -> r
      VIdent r _ -> r
      VList r _ -> r
      VString r _ -> r

      VDoc r _ -> r
      VConstruct r _ -> r
      VBlock r _ -> r

traverseValue :: Monad m => (Value -> m Value) -> Value -> m Value
traverseValue f v =
  case v of
    VDouble {} -> f v
    VInt {} -> f v
    VIdent {} -> f v
    VString {} -> f v
    VList r l ->
      (VList r <$> traverseValue f `traverse` l) >>= f

    VDoc {} -> f v
    VConstruct r c ->
      do  v' <- traverseValue f `traverseAssoc` tagValue c
          f (VConstruct r $ c { tagValue = v'})

    VBlock r b ->
      do  v' <- traverseValue f `traverseAssoc` blockValues b
          f (VBlock r $ b { blockValues = v' })


mapSelected :: Applicative f => (Value -> f Value) -> [Ident] -> Value -> f Value
mapSelected f path v =
  case path of
    [] -> f v
    n:r ->
      case v of
        VBlock loc b ->
          VBlock loc . mkBlock b <$> (mapElt n r `traverse` blockValues b)
        VConstruct loc c | unloc (tagTag c) == n ->
          VConstruct loc . mkCns c <$> (mapElt n r `traverse` tagValue c)
        _ -> pure v
  where
    mkBlock b elts' = b { blockValues = elts' }
    mkCns c elts' = c { tagValue = elts' }

    mapElt p r (n, v') | unloc n == p =  (n,) <$> mapSelected f r v'
                       | otherwise = pure (n, v')

traverseSchemaValues :: Monad m => (Value -> m Value) -> Ident -> Value -> m Value
traverseSchemaValues f i = traverseValue sch
  where
    sch v =
      case v of
        VBlock _ b | blockSchema b == Just i -> f v
        VConstruct _ t | tagSchema t == Just i -> f v
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

validateValue :: Schema.SType -> Value -> Val Value
validateValue ty val =
  case val of
    VDouble {} -> req Schema.SFloat
    VInt {} -> req Schema.SInt
    VIdent {} -> req Schema.SIdent
    VString {} -> req Schema.SString
    VList r elts ->
      case ty of
        Schema.SList ty' ->
          VList r <$> (validateValue ty' `traverse` elts)
        _ -> unexpected "list"
    VDoc {} -> unexpected "doc"
    VConstruct loc cns ->
      do  n <- reqNamed "union constructor"
          union <- getUnion n
          fieldTys <- constructorFields cns union
          fvals' <- validateConstructorField cns `traverse` Map.toList fieldTys
          flagExtraFieldError fieldTys `traverse_` tagValue cns
          pure $ VConstruct loc (cns { tagValue = fvals', tagSchema = Just n })
    VBlock loc block ->
      do  n <- reqNamed "block"
          blockS <- getBlock n
          let fieldTys = Schema.blockSFields blockS
          fvals' <- concat <$> (validateBlockField block `traverse` Map.toList fieldTys)
          flagSuperfluousBlockElt fieldTys `traverse_` blockValues block
          pure $ VBlock loc (block { blockValues = fvals', blockSchema = Just n})

  where
    -- block stuff
    validateBlockField block (n, bd) =
      do  let gty = unloc . Schema.declType . Schema.blockDeclDecl <$> bd
              bNVals = lookupLocated n (blockValues block)
              bVals = snd <$> bNVals
              bNs = fst <$> bNVals
          vbVals' <- zip bNs <$> (validateValue (Schema.unGlob gty) `traverse` bVals)
          case (gty, vbVals') of
            (Schema.One {}, [v]) -> pure [v]
            (Schema.One {}, _) ->
              throw val ("Expecting exactly one " <> n <> " in this block")
            (Schema.Some {}, []) ->
              throw val ("Expecting at least one " <> n <> " in this block")
            (Schema.Some {}, vs) -> pure vs
            (Schema.Many {}, vs) -> pure vs
            (Schema.Optional {}, []) -> pure []
            (Schema.Optional {}, [v]) -> pure [v]
            (Schema.Optional {}, _) ->
              throw val ("Expecting at most one " <> n <> " in this block")

    flagSuperfluousBlockElt fieldTys (ln, _) =
      case Map.lookup (unloc ln) fieldTys of
        Nothing -> throw ln (unloc ln <> " is not part of this block")
        Just _ -> pure ()

    -- constructor stuff
    flagExtraFieldError fieldTys (ln, _) =
      case Map.lookup (unloc ln) fieldTys of
        Nothing -> throw ln ("Field " <> unloc ln <> " is not part of this union")
        Just _ -> pure ()

    validateConstructorField c (n,ty') =
      case lookupLocated n (tagValue c) of
        [] -> throw val ("Constructor " <> q (unloc $ tagTag c) <> " is missing required value " <>
                         q n)
        _:(b,_):_ -> throw b ("Field " <> q (unloc b) <> " appears more than once in constructor")
        [(lname, val')] ->
          do  val'' <- validateValue ty' val'
              pure (lname, val'')

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
            Schema.UnionDef u -> pure u
            Schema.BlockDef {} -> unexpected "union constructor"

    getBlock n =
      do  def <- getSchemaDef val n
          case def of
            Schema.UnionDef {} -> unexpected "block"
            Schema.BlockDef b -> pure b

    reqNamed ue =
      case ty of
        Schema.SNamed n -> pure n
        _ -> unexpected ue

    unexpected n =  throw val ("Not expecting " <> n <> " here - should be a value of type " <> q (showT ty))
    req ty' = requireType val ty ty' >> pure val

requireType :: HasLocation why => why -> Schema.SType -> Schema.SType -> Val ()
requireType why expected actual =
    if expected == actual
    then pure ()
    else throw why ("Expected " <> q (showT expected) <> " but actual type here is " <> q (showT actual))

--

-- TODO: should we just parse to the `Value` in this module?

blockValueToValue :: Blok.Value -> Value
blockValueToValue e =
  case e of
    Blok.Number n -> VDouble (location n) (unloc n)
    Blok.Ident i -> VIdent (location i) (unloc i)
    Blok.String s -> VString (location s) (unloc s)
    Blok.List l -> VList (location l)  (blockValueToValue <$> unloc l)
    Blok.Construct cns ->
      let c = unloc cns in
      VConstruct (location cns)
        TagValue { tagRenderer = Nothing
                 , tagSchema = Nothing
                 , tagTag = Blok.constructorName c
                 , tagValue = fmap blockValueToValue <$> Blok.constructorFields c
                 }

blockBlockToValue :: Located Blok.Block -> Value
blockBlockToValue block =
  let b = unloc block in
  VBlock (location block)
    BlockValue { blockRenderer = Nothing
               , blockSchema = Nothing
               , blockType = Blok.blockTypeName b
               , blockName = Blok.blockName b
               , blockValues = eltVal <$> Blok.blockContents b
               }
  where
    eltVal elt =
      case elt of
        Blok.BlockSub block' ->
          let bv = blockBlockToValue block' in (Blok.blockTypeName (unloc block'), bv)
        Blok.BlockValue name v -> (name, blockValueToValue v)

--

traverseAssoc :: Applicative f => (b -> f c) -> [(a,b)] -> f [(a, c)]
traverseAssoc f l = tr `traverse` l
  where
    tr (a, b) = (a,) <$> f b

lookupLocated :: Eq a => a -> [(Located a, b)] -> [(Located a, b)]
lookupLocated key lst =  [(la, b) | (la, b) <- lst, unloc la == key ]

showT :: Show a => a -> Text
showT = Text.pack . show

q :: Text -> Text
q a = "'" <> a <> "'"