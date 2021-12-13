module Data.Iris.App.Internal.Resolving.NamedResolver
  ( runResolverMap,
  )
where

import Data.Iris.App.Internal.Resolving.Event (EventHandler (Channel))
import Data.Iris.App.Internal.Resolving.ResolveValue
  ( resolveRef,
  )
import Data.Iris.App.Internal.Resolving.Resolver (LiftOperation, Resolver, ResponseStream, runResolver)
import Data.Iris.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverState,
  )
import Data.Iris.App.Internal.Resolving.Types
  ( NamedResolverRef (..),
    ResolverMap,
  )
import Language.Iris.Types.Internal.AST
  ( Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName,
    VALID,
    ValidValue,
    Value (..),
  )

runResolverMap ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  TypeName ->
  ResolverMap (Resolver o e m) ->
  ResolverContext ->
  SelectionSet VALID ->
  ResponseStream e m ValidValue
runResolverMap
  channels
  name
  res
  ctx
  selection = runResolver channels resolvedValue ctx
    where
      resolvedValue = resolveRef res (NamedResolverRef name Null) (SelectionSet selection)
