{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Parsing.Request.Parser
  ( parseRequest,
  )
where

import qualified Data.Aeson as Aeson
  ( Value (..),
  )
import Data.HashMap.Lazy (toList)
import Data.Mergeable.Utils
  ( IsMap (unsafeFromList),
    empty,
    fromElems,
    toLBS,
  )
import Language.Iris.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Language.Iris.Parsing.Internal.Terms
  ( ignoredTokens,
  )
import Language.Iris.Parsing.Request.Operation
  ( parseOperation,
  )
import Language.Iris.Parsing.Request.Selection
  ( parseFragmentDefinition,
  )
import Language.Iris.Types.IO (GQLRequest (..))
import Language.Iris.Types.Internal.AST
  ( ExecutableDocument (..),
    GQLResult,
    Variables,
    packName,
    replaceValue,
  )
import Relude hiding
  ( empty,
    fromList,
    many,
    toList,
  )
import Text.Megaparsec
  ( eof,
    label,
    many,
  )

parseExecutableDocument :: Variables -> Parser ExecutableDocument
parseExecutableDocument variables =
  label "ExecutableDocument" $
    ( ExecutableDocument variables
        <$> (ignoredTokens *> parseOperation)
        <*> (many parseFragmentDefinition >>= lift . fromElems)
    )
      <* ignoredTokens
      <* eof

parseRequest :: GQLRequest -> GQLResult ExecutableDocument
parseRequest GQLRequest {query, variables} =
  processParser
    (toVariables variables >>= parseExecutableDocument)
    (toLBS query)
  where
    toVariables :: MonadFail m => Maybe Aeson.Value -> m Variables
    toVariables (Just (Aeson.Object entries)) = unsafeFromList <$> traverse toMorpheusValue (toList entries)
      where
        toMorpheusValue (key, value) = (packName key,) <$> replaceValue value
    toVariables _ = pure empty
