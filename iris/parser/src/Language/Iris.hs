{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris
  ( Config (..),
    RenderGQL (..),
    VALIDATION_MODE (..),
    ValidateSchema (..),
    debugConfig,
    defaultConfig,
    internalSchema,
    parseFullSchema,
    parseRequest,
    parseRequestWith,
    parseSchema,
    render,
    validateRequest,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Mergeable.Utils
  ( 
    Result,
  )
import Language.Iris.Parser
  ( parseRequest,
    parseRequestWith,
    parseSchema,
  )
import Language.Iris.Rendering.RenderGQL (RenderGQL (..), render)
import Language.Iris.Schema.Schema (internalSchema)
import Language.Iris.Types.Internal.AST
  ( GQLError,
    Schema,
    VALID, (<:>),
  )
import Language.Iris.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
    debugConfig,
    defaultConfig,
  )
import Language.Iris.Validation.Document.Validation (ValidateSchema (..))
import Language.Iris.Validation.Query.Validation
  ( validateRequest,
  )
import Relude hiding (ByteString)

parseFullSchema :: ByteString -> Result GQLError (Schema VALID)
parseFullSchema = parseSchema >=> (internalSchema <:>)
