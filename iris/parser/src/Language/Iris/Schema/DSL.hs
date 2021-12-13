{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Schema.DSL (dsl) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8
  ( ByteString,
    pack,
    unpack,
  )
import Data.Mergeable.Utils.Result
  ( Result (..),
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Iris.Parsing.Document.TypeSystem
  ( parseSchema,
  )
import Language.Iris.Types.Internal.AST (GQLErrors)
import Relude hiding (ByteString)

dsl :: QuasiQuoter
dsl =
  QuasiQuoter
    { quoteExp = dslExpression . pack,
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = notHandled "Declarations"
    }
  where
    notHandled things = error $ things <> " are not supported by the GraphQL QuasiQuoter"

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode . toList

dslExpression :: ByteString -> Q Exp
dslExpression doc = case parseSchema doc of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result} -> [|result|]
