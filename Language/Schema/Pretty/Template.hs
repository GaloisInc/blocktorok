{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Language.Schema.Pretty.Template
Description : Blocktorok data template generation from schemas
Copyright   : (c) Galois, Inc. 2021
License     : N/A
Maintainer  : cphifer@galois.com
Stability   : experimental
Portability : N/A

Generation of pretty-printed Blocktorok data templates from a Blocktorok
schema. This is an experimental feature that will certainly require tuning to
make sure the generated template is correct.
-}

module Language.Schema.Pretty.Template
  ( ppSchemaTemplate
  ) where

import           Control.Monad.Reader      (MonadReader (ask), Reader, asks,
                                            runReader)

import           Data.Text                 (Text)

import           Prettyprinter             (Doc, Pretty (pretty), concatWith,
                                            hardline, indent, lbrace, rbrace,
                                            (<+>))
import           Prettyprinter.Render.Text (putDoc)

import qualified System.FilePath           as Path

import           Language.Common           (orThrow, unloc)
import           Language.Common.Pretty    (ppStype, (<//>), (</>))
import           Language.Schema.Env       (Env (..), lookupTypeDef)
import           Language.Schema.Parser    (schemaEnvFromFile)
import           Language.Schema.Syntax    (BlockDecl (..), BlockS (..),
                                            Decl (..), SchemaDef (..))
import           Language.Schema.Type      (Globbed (..), Ident, SType (..))

import           Link                      (LinkError (..))

-- | Generate and print a data template from a schema file
ppSchemaTemplate :: FilePath -> IO ()
ppSchemaTemplate fp =
  do env <- schemaEnvFromFile fp `orThrow` ParseError
     let templ = comment "Template generated from"
             <+> pretty (Path.dropExtensions fp)
            <//> runReader ppTemplate env
              <> hardline
     putDoc templ

-------------------------------------------------------------------------------
-- Generating templates from 'Schema's

ppTemplate :: Reader Env (Doc ann)
ppTemplate =
  do rootTys <- asks envRootTypes
     fs <- mapM ppField rootTys
     pure $ concatWith (<//>) fs
  where
    ppField :: Globbed BlockDecl -> Reader Env (Doc ann)
    ppField gbd = case gbd of
      One bd      -> ppDeclWith "Must appear exactly once." bd
      Optional bd -> ppDeclWith "Must appear at most once." bd
      Some bd     -> ppDeclWith "Must appear at least once." bd
      Many bd     -> ppDeclWith "Appears any number of times." bd

    ppDeclWith :: Text -> BlockDecl -> Reader Env (Doc ann)
    ppDeclWith msg BlockDecl { blockDeclDecl = Decl { declName, declType } } =
      do templ <- ppDeclType (unloc declName) (unloc declType)
         pure $ comment "Type:" <+> (ppStype . unloc) declType <> "."
            <+> pretty msg
            </> templ

    ppDeclType :: Ident -> SType -> Reader Env (Doc ann)
    ppDeclType nm ty =
      do env <- ask
         case ty of
           SNamed nm' ->
             case lookupTypeDef nm' env of
               Nothing -> error "[BUG] Missing type definition"
               Just (UnionDef _) ->
                 pure $ comment "See the documentation for variant details."
                    </> pretty nm <> ":"
               Just (BlockDef BlockS { blockSFields }) ->
                 do fs <- concatWith (<//>) <$> mapM ppField blockSFields
                    pure $ pretty nm <> ":" <+> lbrace </> indent 2 fs </> rbrace
           _ -> pure $ pretty nm <> ":"

-------------------------------------------------------------------------------
-- Conveniences

comment :: Doc ann -> Doc ann
comment = ("--" <+>)
