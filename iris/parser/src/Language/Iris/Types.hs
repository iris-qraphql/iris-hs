module Language.Iris.Types
  ( EncodeScalar (..),
    DecodeScalar (..),
    toScalar,
    scalarToJSON,
    scalarFromJSON,
    scalarValidator,
    GQLRequest (..),
    GQLResponse (..),
    renderResponse,ID (..),
  )
where

import Language.Iris.Types.GQLScalar
import Language.Iris.Types.IO
import Language.Iris.Types.ID