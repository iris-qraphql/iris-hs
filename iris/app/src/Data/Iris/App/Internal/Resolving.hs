{-# LANGUAGE NoImplicitPrelude #-}

module Data.Iris.App.Internal.Resolving
  ( Resolver,
    LiftOperation,
    runRootResolverValue,
    lift,
    ResponseEvent (..),
    ResponseStream,
    cleanEvents,
    Result (..),
    ResultT (..),
    ObjectTypeResolver (..),
    WithOperation,
    PushEvents (..),
    subscribe,
    ResolverContext (..),
    unsafeInternalContext,
    RootResolverValue (..),
    resultOr,
    withArguments,
    -- Dynamic Resolver
    mkBoolean,
    mkFloat,
    mkInt,
    mkList,
    mkNull,
    mkString,
    mkValue,
    mkObject,
    SubscriptionField (..),
    getArguments,
    ResolverState,
    liftResolverState,
    ResolverEntry,
    sortErrors,
    EventHandler (..),
    requireObject,
    ResolverValue (..),
    NamedResolver (..),
    NamedResolverResult (..),
    NamedResolverRef (..),
  )
where

import Data.Iris.App.Internal.Resolving.Event
import Data.Iris.App.Internal.Resolving.Resolver
import Data.Iris.App.Internal.Resolving.ResolverState
import Data.Iris.App.Internal.Resolving.RootResolverValue
import Data.Iris.App.Internal.Resolving.Types
import Data.Iris.App.Internal.Resolving.Utils
import Data.Mergeable.Utils
import Data.Iris.App.Internal.Resolving.Result
