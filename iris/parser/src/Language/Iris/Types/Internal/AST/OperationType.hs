{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.OperationType
  ( OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
  )
where

import Data.Char (toLower)
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Language.Iris.Types.Internal.AST.Error (Msg (..))
import Language.Iris.Types.Internal.AST.Name (TypeName)
import Language.Haskell.TH.Syntax
  ( Lift,
  )
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

type QUERY = 'Query

type MUTATION = 'Mutation

type SUBSCRIPTION = 'Subscription

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving
    ( Show,
      Eq,
      Lift,
      Generic,
      Hashable
    )

instance RenderGQL OperationType where
  renderGQL = fromString . fmap toLower . show

instance Msg OperationType where
  msg Query = msg ("query" :: TypeName)
  msg Mutation = msg ("mutation" :: TypeName)
  msg Subscription = msg ("subscription" :: TypeName)
