{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Iris.App.Internal.Resolving.Event
  ( EventHandler (..),
    ResponseEvent (..),
  )
where

import Data.Kind (Type)
import Language.Iris.Types ( GQLResponse)

class EventHandler e where
  type Channel e
  getChannels :: e -> [Channel e]

data ResponseEvent event (m :: Type -> Type)
  = Publish event
  | Subscribe
      { subChannel :: Channel event,
        subRes :: event -> m GQLResponse
      }
