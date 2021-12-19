{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Parsing.Document.TypeSystem
  ( parseSchema,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Mergeable.Utils
  ( empty,
    fromElems,
  )
import Language.Iris.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Language.Iris.Parsing.Internal.Pattern
  ( argumentsDefinition,
    fieldsDefinition,
    optionalDirectives,
    parseDirectiveLocation,
    typeDeclaration,
    typeGuard,
    unionMembersDefinition,
  )
import Language.Iris.Parsing.Internal.Terms
  ( at,
    equal,
    ignoredTokens,
    keyword,
    optDescription,
    optionalCollection,
    parseName,
    pipe,
  )
import Language.Iris.Parsing.Internal.Value
  ( Parse (..),
  )
import Language.Iris.Types.Internal.AST
  ( CONST,
    Description,
    DirectiveDefinition (..),
    GQLResult,
    ListDefinition (..),
    RESOLVER_TYPE,
    RawTypeDefinition (..),
    ScalarDefinition (..),
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    Value,
    Variant (..),
    mkSchema,
  )
import Relude hiding (ByteString, empty)
import Text.Megaparsec
  ( eof,
    label,
    manyTill,
  )

scalarTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition RESOLVER_TYPE s)
scalarTypeDefinition description =
  label "ScalarTypeDefinition" $
    TypeDefinition description
      <$> typeDeclaration "scalar"
      <*> optionalDirectives
      <*> pure (ScalarTypeContent (ScalarDefinition pure))
{-# INLINEABLE scalarTypeDefinition #-}

listTypeDefinition :: Maybe Description -> Parser ListDefinition
listTypeDefinition description =
  label "ListTypeDefinition" $
    ListDefinition description <$> typeDeclaration "list"
{-# INLINEABLE listTypeDefinition #-}

resolverTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition RESOLVER_TYPE s)
resolverTypeDefinition description =
  label "ResolverTypeDefinition" $ do
    name <- typeDeclaration "resolver"
    TypeDefinition
      description
      name
      <$> optionalDirectives
      <*> (content name <|> pure (typeVariant name empty))
  where
    typeVariant n = ResolverTypeContent Nothing . (:| []) . Variant Nothing n Nothing
    content name = do
      tyGuard <- typeGuard
      equal
        *> ( (typeVariant name <$> fieldsDefinition)
               <|> (ResolverTypeContent tyGuard <$> unionMembersDefinition name)
           )
{-# INLINEABLE resolverTypeDefinition #-}

dataTypeDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (TypeDefinition RESOLVER_TYPE s)
dataTypeDefinition description =
  label "DataTypeDefinition" $ do
    name <- typeDeclaration "data"
    TypeDefinition
      description
      name
      <$> optionalDirectives
      <*> ( equal
              *> ( fmap DataTypeContent (unionMembersDefinition name)
                     <|> fmap (typeVariant name) (fieldsDefinition <|> pure empty)
                 )
          )
  where
    typeVariant name = DataTypeContent . (:| []) . Variant Nothing name Nothing
{-# INLINEABLE dataTypeDefinition #-}

parseDirectiveDefinition ::
  Parse (Value s) =>
  Maybe Description ->
  Parser (DirectiveDefinition s)
parseDirectiveDefinition description =
  label "DirectiveDefinition" $
    DirectiveDefinition
      <$> ( keyword "directive"
              *> at
              *> parseName
          )
        <*> pure description
        <*> optionalCollection argumentsDefinition
        <*> (optional (keyword "repeatable") *> keyword "on" *> pipe parseDirectiveLocation)
{-# INLINEABLE parseDirectiveDefinition #-}

parseTypeSystemUnit ::
  Parser RawTypeDefinition
parseTypeSystemUnit =
  label "TypeDefinition" $ do
    description <- optDescription
    RawTypeDefinition
      <$> ( scalarTypeDefinition description
              <|> dataTypeDefinition description
              <|> resolverTypeDefinition description
          )
      <|> (RawDirectiveDefinition <$> parseDirectiveDefinition description)
      <|> (RawListDefinition <$> listTypeDefinition description)
{-# INLINEABLE parseTypeSystemUnit #-}

parseRawTypeDefinitions :: Parser [RawTypeDefinition]
parseRawTypeDefinitions =
  label "TypeSystemDefinitions" $
    ignoredTokens
      *> manyTill parseTypeSystemUnit eof

parseSchema :: ByteString -> GQLResult (Schema CONST)
parseSchema src = do
  defs <- processParser parseRawTypeDefinitions src
  dirs <- fromElems [dir | RawDirectiveDefinition dir <- defs]
  mkSchema [t | RawTypeDefinition t <- defs] dirs
