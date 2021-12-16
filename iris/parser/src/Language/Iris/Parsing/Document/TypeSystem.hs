{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Parsing.Document.TypeSystem
  ( parseSchema,
    parseTypeDefinitions,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldr')
import Data.Mergeable (NameCollision (nameCollision), throwErrors)
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
    parseOperationType,
    typeDeclaration,
    typeGuard,
    unionMembersDefinition,
  )
import Language.Iris.Parsing.Internal.Terms
  ( at,
    colon,
    equal,
    ignoredTokens,
    keyword,
    optDescription,
    optionalCollection,
    parseName,
    parseTypeName,
    pipe,
    setOf,
  )
import Language.Iris.Parsing.Internal.Value
  ( Parse (..),
  )
import Language.Iris.Types.Internal.AST
  ( CONST,
    Description,
    DirectiveDefinition (..),
    DirectivesDefinition,
    GQLResult,
    RESOLVER_TYPE,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    ScalarDefinition (..),
    Schema,
    SchemaDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
    UnionMember (..),
    Value,
    buildSchema,
  )
import Relude hiding (ByteString, empty)
import Text.Megaparsec
  ( eof,
    label,
    manyTill,
  )

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
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

--
--  ResolverTypeDefinition:
--    Description(opt) type Name Directives(Const)(opt) FieldsDefinition(opt)
--
--  ResolverTypeContent  =
--    - FieldDefinition(list)
--    - UnionMemberTypes
--
--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
--  UnionTypeDefinition:
--    Description(opt) type Name Directives(Const)(opt) UnionMemberTypes(opt)
--
--  UnionMemberTypes = UnionMemberTypes | NamedType
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
      <*> (content name <|> pure (typeVariant name  empty))
  where
    typeVariant n = ResolverTypeContent Nothing . (:| []) . UnionMember Nothing n Nothing
    content name = do
      tyGuard <- typeGuard
      equal
        *> ( (typeVariant name <$> fieldsDefinition)
               <|> (ResolverTypeContent tyGuard <$> unionMembersDefinition name)
           )

{-# INLINEABLE resolverTypeDefinition #-}

-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   DataTypeDefinition
--     Description(opt) data Name  Directives(Const)(opt) dataFieldsDefinition(opt)
--
--   dataFieldsDefinition:
--     { InputValueDefinition(list) }
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
    typeVariant name = DataTypeContent . (:| []) . UnionMember Nothing name Nothing
{-# INLINEABLE dataTypeDefinition #-}

-- 3.13 DirectiveDefinition
--
--  DirectiveDefinition:
--     Description[opt] directive @ Name ArgumentsDefinition[opt] repeatable[opt] on DirectiveLocations
--
--  DirectiveLocations:
--    DirectiveLocations | DirectiveLocation
--    |[opt] DirectiveLocation
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

-- 3.2 Schema
-- SchemaDefinition:
--    schema Directives[Const,opt]
--      { RootOperationTypeDefinition(list) }
--
--  RootOperationTypeDefinition:
--    OperationType: NamedType

-- data SchemaDefinition = SchemaDefinition
--   { query :: TypeName,
--     mutation :: Maybe TypeName,
--     subscription :: Maybe TypeName
--   }
parseSchemaDefinition :: Maybe Description -> Parser SchemaDefinition
parseSchemaDefinition _schemaDescription =
  label "SchemaDefinition" $
    keyword "schema"
      *> ( SchemaDefinition
             <$> optionalDirectives
             <*> setOf parseRootOperationTypeDefinition
         )
{-# INLINEABLE parseSchemaDefinition #-}

parseRootOperationTypeDefinition :: Parser RootOperationTypeDefinition
parseRootOperationTypeDefinition =
  RootOperationTypeDefinition
    <$> (parseOperationType <* colon)
    <*> parseTypeName
{-# INLINEABLE parseRootOperationTypeDefinition #-}

parseTypeSystemUnit ::
  Parser RawTypeDefinition
parseTypeSystemUnit =
  label "TypeDefinition" $
    do
      description <- optDescription
      parseTypeDef description
        <|> RawSchemaDefinition <$> parseSchemaDefinition description
        <|> RawDirectiveDefinition <$> parseDirectiveDefinition description
  where
    parseTypeDef description =
      RawTypeDefinition
        <$> ( scalarTypeDefinition description
                <|> dataTypeDefinition description
                <|> resolverTypeDefinition description
            )
{-# INLINEABLE parseTypeSystemUnit #-}

typePartition ::
  [RawTypeDefinition] ->
  ( [SchemaDefinition],
    [TypeDefinition RESOLVER_TYPE CONST],
    [DirectiveDefinition CONST]
  )
typePartition = foldr' split ([], [], [])

split ::
  RawTypeDefinition ->
  ( [SchemaDefinition],
    [TypeDefinition RESOLVER_TYPE CONST],
    [DirectiveDefinition CONST]
  ) ->
  ( [SchemaDefinition],
    [TypeDefinition RESOLVER_TYPE CONST],
    [DirectiveDefinition CONST]
  )
split (RawSchemaDefinition schema) (schemas, types, dirs) = (schema : schemas, types, dirs)
split (RawTypeDefinition ty) (schemas, types, dirs) = (schemas, ty : types, dirs)
split (RawDirectiveDefinition dir) (schemas, types, dirs) = (schemas, types, dir : dirs)

--  split (RawDirectiveDefinition d)

withSchemaDefinition ::
  ( [SchemaDefinition],
    [TypeDefinition RESOLVER_TYPE s],
    [DirectiveDefinition CONST]
  ) ->
  GQLResult (Maybe SchemaDefinition, [TypeDefinition RESOLVER_TYPE s], DirectivesDefinition CONST)
withSchemaDefinition ([], t, dirs) = (Nothing,t,) <$> fromElems dirs
withSchemaDefinition ([x], t, dirs) = (Just x,t,) <$> fromElems dirs
withSchemaDefinition (x : xs, _, _) = throwErrors (nameCollision <$> (x :| xs))

parseRawTypeDefinitions :: Parser [RawTypeDefinition]
parseRawTypeDefinitions =
  label "TypeSystemDefinitions" $
    ignoredTokens
      *> manyTill parseTypeSystemUnit eof

typeSystemDefinition ::
  ByteString ->
  GQLResult
    ( Maybe SchemaDefinition,
      [TypeDefinition RESOLVER_TYPE CONST],
      DirectivesDefinition CONST
    )
typeSystemDefinition =
  processParser parseRawTypeDefinitions
    >=> withSchemaDefinition . typePartition

parseTypeDefinitions :: ByteString -> GQLResult [TypeDefinition RESOLVER_TYPE CONST]
parseTypeDefinitions =
  fmap (\d -> [td | RawTypeDefinition td <- d])
    . processParser parseRawTypeDefinitions

parseSchema :: ByteString -> GQLResult (Schema CONST)
parseSchema = typeSystemDefinition >=> buildSchema
