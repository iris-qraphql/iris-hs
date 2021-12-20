{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Iris.Parsing.Internal.Pattern
  ( fieldsDefinition,
    typeDeclaration,
    optionalDirectives,
    parseOperationType,
    argumentsDefinition,
    parseDirectiveLocation,
    unionMembersDefinition,
    typeGuard,
  )
where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Mergeable.Utils (empty, fromElems)
import Language.Iris.Parsing.Internal.Arguments
  ( maybeArguments,
  )
import Language.Iris.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Language.Iris.Parsing.Internal.Terms
  ( at,
    colon,
    ignoredTokens,
    keyword,
    optDescription,
    parseName,
    parseType,
    parseTypeName,
    pipe,
    setOf,
    symPipe,
    uniqTuple,
  )
import Language.Iris.Parsing.Internal.Value
  ( Parse (..),
    parseDefaultValue,
  )
import Language.Iris.Types.Internal.AST
  ( ArgumentDefinition (..),
    ArgumentsDefinition,
    DATA_TYPE,
    Directive (..),
    DirectiveLocation (..),
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldsDefinition,
    OperationType (..),
    RESOLVER_TYPE,
    TypeName,
    Value,
    Variant (..),
    Variants,
  )
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( choice,
    label,
    many,
  )
import Text.Megaparsec.Byte (string)

unionMembersDefinition ::
  (Parse (Value s), Parse (FieldContent cat s)) =>
  TypeName ->
  Parser (Variants cat s)
unionMembersDefinition typeName = label "Variant" $ pipe (parseMember typeName)

parseMember ::
  (Parse (Value s), Parse (FieldContent cat s)) =>
  TypeName ->
  Parser (Variant cat s)
parseMember typeName = do
  variantDescription <- optDescription
  variantName <- parseTypeName
  fields <- optional fieldsDefinition
  pure
    Variant
      { variantFields = fromMaybe empty fields,
        membership = fmap (const typeName) fields,
        ..
      }

{-# INLINEABLE unionMembersDefinition #-}

argumentsDefinition ::
  Parse (Value s) =>
  Parser (ArgumentsDefinition s)
argumentsDefinition =
  label "ArgumentsDefinition" $
    uniqTuple $ do
      fieldDescription <- optDescription
      fieldName <- parseName
      fieldType <- colon *> parseType
      argumentDefaultValue <- optional parseDefaultValue
      fieldDirectives <- optionalDirectives
      pure
        ArgumentDefinition
          { argument = FieldDefinition {fieldContent = Nothing, ..},
            argumentDefaultValue
          }
{-# INLINEABLE argumentsDefinition #-}

--  FieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#FieldsDefinition
--
--  FieldsDefinition :
--    { FieldDefinition(list) }
--
fieldsDefinition ::
  (Parse (Value s), Parse (FieldContent cat s)) =>
  Parser (FieldsDefinition cat s)
fieldsDefinition = label "FieldsDefinition" $ setOf fieldDefinition
{-# INLINEABLE fieldsDefinition #-}

fieldDefinition ::
  (Parse (Value s), Parse (FieldContent cat s)) =>
  Parser (FieldDefinition cat s)
fieldDefinition =
  label "FieldDefinition" $
    FieldDefinition
      <$> optDescription
      <*> parseName
      <*> optional parse
      <*> (colon *> parseType)
      <*> optionalDirectives

instance Parse (Value s) => Parse (FieldContent DATA_TYPE s) where
  parse = pure DataFieldContent

instance Parse (Value s) => Parse (FieldContent RESOLVER_TYPE s) where
  parse = ResolverFieldContent <$> argumentsDefinition

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)

-- Directives : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--
-- example: @directive ( arg1: "value" , .... )
--
-- Directives[Const]
-- Directive[Const](list)
--
optionalDirectives :: Parse (Value s) => Parser (Directives s)
optionalDirectives = label "Directives" $ many directive >>= lift . fromElems
{-# INLINEABLE optionalDirectives #-}

-- Directive[Const]
--
-- @ Name Arguments[Const](opt)
directive :: Parse (Value s) => Parser (Directive s)
directive =
  label "Directive" $
    Directive
      <$> getLocation
      <*> (at *> parseName)
      <*> maybeArguments
{-# INLINEABLE directive #-}

-- typDeclaration : Not in spec ,start part of type definitions
--
--  typDeclaration
--   Description(opt) scalar Name
--
typeDeclaration :: ByteString -> Parser TypeName
typeDeclaration kind = keyword kind *> parseTypeName
{-# INLINEABLE typeDeclaration #-}

parseOperationType :: Parser OperationType
parseOperationType =
  label "OperationType" $
    ( (string "query" $> Query)
        <|> (string "mutation" $> Mutation)
        <|> (string "subscription" $> Subscription)
    )
      <* ignoredTokens
{-# INLINEABLE parseOperationType #-}

parseDirectiveLocation :: Parser DirectiveLocation
parseDirectiveLocation =
  label
    "DirectiveLocation"
    ( choice
        ( map
            toKeyword
            [ FIELD_DEFINITION,
              FRAGMENT_DEFINITION,
              FRAGMENT_SPREAD,
              INLINE_FRAGMENT,
              ARGUMENT_DEFINITION,
              DATA_FIELD_DEFINITION,
              QUERY,
              MUTATION,
              SUBSCRIPTION,
              FIELD,
              DATA,
              SCALAR,
              RESOLVER
            ]
        )
    )
    <* ignoredTokens
{-# INLINEABLE parseDirectiveLocation #-}

toKeyword :: Show a => a -> Parser a
toKeyword x = string (fromString $ show x) $> x
{-# INLINEABLE toKeyword #-}

typeGuard :: Parser (Maybe TypeName)
typeGuard = label "TypeGuard" $ optional (symPipe *> parseTypeName)
