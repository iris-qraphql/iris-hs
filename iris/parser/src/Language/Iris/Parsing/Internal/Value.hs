{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Parsing.Internal.Value
  ( parseDefaultValue,
    Parse (..),
  )
where

import Data.Mergeable.Utils (empty)
import Language.Iris.Parsing.Internal.Internal
  ( Parser,
  )
import Language.Iris.Parsing.Internal.Terms
  ( brackets,
    colon,
    equal,
    ignoredTokens,
    parseName,
    parseString,
    parseTypeName,
    setOf,
    symbol,
    variable,
  )
import Language.Iris.Types.Internal.AST
  ( CONST,
    FieldName,
    ObjectEntry (..),
    OrdMap,
    RAW,
    ScalarValue (..),
    Value (..),
    decodeScientific,
  )
import Relude hiding (empty)
import Text.Megaparsec
  ( label,
    sepBy,
  )
import Text.Megaparsec.Byte
  ( string,
  )
import Text.Megaparsec.Byte.Lexer (scientific)

-- '-'


valueNull :: Parser (Value a)
valueNull = string "null" $> Null
{-# INLINE valueNull #-}

booleanValue :: Parser (Value a)
booleanValue =
  Scalar . Boolean
    <$> ( string "true" $> True
            <|> string "false" $> False
        )
{-# INLINE booleanValue #-}

valueNumber :: Parser (Value a)
valueNumber = Scalar . decodeScientific <$> ((*) <$> negation <*> scientific)
  where
    negation = (symbol 45 $> (-1) <* ignoredTokens) <|> pure 1
    {-# INLINE negation #-}
{-# INLINE valueNumber #-}

stringValue :: Parser (Value a)
stringValue = Scalar . String <$> parseString
{-# INLINE stringValue #-}

listValue :: Parser a -> Parser [a]
listValue parser = label "List" $ brackets (parser `sepBy` ignoredTokens)
{-# INLINE listValue #-}

objectEntry :: Parser (Value a) -> Parser (ObjectEntry a)
objectEntry parser = ObjectEntry <$> (parseName <* colon) <*> parser
{-# INLINE objectEntry #-}

objectValue :: Parser (Value a) -> Parser (OrdMap FieldName (ObjectEntry a))
objectValue = label "ObjectValue" . setOf . objectEntry
{-# INLINE objectValue #-}

parsePrimitives :: Parser (Value a)
parsePrimitives =
  valueNull
    <|> booleanValue
    <|> valueNumber
    <|> stringValue
{-# INLINE parsePrimitives #-}

parseDefaultValue :: Parser (Value s)
parseDefaultValue = equal *> parseV
  where
    parseV :: Parser (Value s)
    parseV = structValue parseV

class Parse a where
  parse :: Parser a

instance Parse (Value RAW) where
  parse = (VariableValue <$> variable) <|> structValue parse

instance Parse (Value CONST) where
  parse = structValue parse

structValue :: Parser (Value a) -> Parser (Value a)
structValue parser =
  label "Value" $
    ( parsePrimitives
        <|> (Object  Nothing <$> objectValue parser)
        <|> (Object <$> (Just <$> parseTypeName <* ignoredTokens) <*> (objectValue parser <|> pure empty))
        <|> (List <$> listValue parser)
    )
      <* ignoredTokens