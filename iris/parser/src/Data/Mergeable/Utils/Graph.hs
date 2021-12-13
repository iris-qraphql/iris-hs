{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Utils.Graph
  ( cycleChecking,
    Graph,
    Edges,
  )
where

import Data.List (lookup)
-- import Language.Iris.Types.Internal.AST (Ref (..))
import Relude

type Edges a = (a, [a])

type Graph a = [Edges a]

cycleChecking ::
  (Applicative m, Eq a) =>
  (NonEmpty a -> m ()) ->
  Graph a ->
  m ()
cycleChecking fail' graph = traverse_ checkNode graph
  where
    checkNode (node, _) = cycleCheckingWith graph node [node] fail'

cycleCheckingWith ::
  (Applicative m,  Eq a) =>
  Graph a ->
  a ->
  [a] ->
  (NonEmpty a -> m ()) ->
  m ()
cycleCheckingWith graph parentNode history fail' =
  case lookup parentNode graph of
    Just node -> traverse_ checkNode node
    Nothing -> pure ()
  where
    checkNode node
      | node `elem` history =
        fail' (node :| history)
      | otherwise =
        cycleCheckingWith
          graph
          node
          (history <> [node])
          fail'
