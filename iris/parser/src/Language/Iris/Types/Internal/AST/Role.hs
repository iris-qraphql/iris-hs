{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Role
  ( Role (..),
    RESOLVER_TYPE,
    DATA_TYPE,
    ToRESOLVER (..),
    ToDATA (..),
  )
where

import Control.Monad.Except (MonadError)
import Language.Iris.Types.Internal.AST.Error (GQLError)
import Language.Iris.Types.Internal.AST.Stage (Stage)
import Relude

data Role = DATA_TYPE | RESOLVER_TYPE deriving (Show, Eq, Ord)

type DATA_TYPE = 'DATA_TYPE

type RESOLVER_TYPE = 'RESOLVER_TYPE

class ToRESOLVER a where
  toRESOLVER :: a k (s :: Stage) -> a RESOLVER_TYPE s

class ToDATA a where
  toDATA :: MonadError GQLError m => a RESOLVER_TYPE (s :: Stage) -> m (a DATA_TYPE s)
