{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.ID
  ( ID (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Hashable
import Language.Iris.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
    scalarFromJSON,
    scalarToJSON,
  )
import Language.Iris.Types.Internal.AST
  ( ScalarValue (..),
  )
import Data.Text (pack)
import Relude

-- | default GraphQL type,
-- parses only 'String' and 'Int' values,
-- serialized always as 'String'
newtype ID = ID
  { unpackID :: Text
  }
  deriving
    ( Show,
      Generic,
      Eq,
      Hashable,
      IsString,
      Semigroup
    )

instance DecodeScalar ID where
  decodeScalar (Int x) = pure (ID $ pack $ show x)
  decodeScalar (String x) = pure (ID x)
  decodeScalar _ = Left "ID can only be String or number"

instance EncodeScalar ID where
  encodeScalar (ID x) = String x

instance ToJSON ID where
  toJSON = scalarToJSON

instance FromJSON ID where
  parseJSON = scalarFromJSON
