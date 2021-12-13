{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Parsing.Internal.Arguments (maybeArguments) where

import Language.Iris.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Language.Iris.Parsing.Internal.Terms
  ( colon,
    parseName,
    uniqTupleOpt,
  )
import Language.Iris.Parsing.Internal.Value
  ( Parse (..),
  )
import Language.Iris.Types.Internal.AST
  ( Argument (..),
    Arguments,
    Value,
  )
import Relude
import Text.Megaparsec (label)

-- Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- Arguments[Const]
-- ( Argument[Const](list) )
--
-- Argument[Const]
--  Name : Value[Const]
valueArgument :: Parse (Value s) => Parser (Argument s)
valueArgument =
  label "Argument" $
    Argument
      <$> getLocation
      <*> (parseName <* colon)
      <*> parse
{-# INLINEABLE valueArgument #-}

maybeArguments :: Parse (Value s) => Parser (Arguments s)
maybeArguments =
  label "Arguments" $
    uniqTupleOpt valueArgument
{-# INLINEABLE maybeArguments #-}
