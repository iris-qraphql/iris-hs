{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Parser
  ( parseSchema,
    parseTypeDefinitions,
    parseRequest,
    parseRequestWith,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Mergeable.Utils
  ( sortErrors,
  )
import Language.Iris.Parsing.Document.TypeSystem
  ( parseTypeDefinitions,
  )
import qualified Language.Iris.Parsing.Document.TypeSystem as P
  ( parseSchema,
  )
import Language.Iris.Parsing.Request.Parser
  ( parseRequest,
  )
import Language.Iris.Schema.Schema (internalSchema)
import Language.Iris.Types.IO
  ( GQLRequest (..),
  )
import Language.Iris.Types.Internal.AST
  ( (<:>),
    GQLResult,
    Operation,
    Schema (..),
    VALID,
  )
import Language.Iris.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
  )
import Language.Iris.Validation.Document.Validation
  ( validateSchema,
  )
import Language.Iris.Validation.Query.Validation
  ( validateRequest,
  )
import Relude hiding (ByteString)

parseSchema ::
  ByteString -> GQLResult (Schema VALID)
parseSchema =
  sortErrors
    . ( P.parseSchema
          >=> validateSchema
            True
            Config
              { debug = False,
                validationMode = FULL_VALIDATION
              }
      )

parseRequestWith :: Config -> Schema VALID -> GQLRequest -> GQLResult (Operation VALID)
parseRequestWith config schema req = do
  qu <- parseRequest req
  fillSchema <- internalSchema <:> schema
  validateRequest config fillSchema qu
