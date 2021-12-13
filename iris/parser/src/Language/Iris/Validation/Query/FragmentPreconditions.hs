{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Query.FragmentPreconditions
  ( checkFragmentPreconditions,
  )
where

import Control.Monad.Except (throwError)
import Data.Mergeable
import Data.Mergeable.Utils
  ( Edges,
    Graph,
    cycleChecking,
    selectOr,
  )
import Language.Iris.Error.Selection
  ( cannotSpreadWithinItself,
  )
import Language.Iris.Types.Internal.AST
  ( Fragment (..),
    FragmentName,
    Fragments,
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
  )
import Language.Iris.Types.Internal.Validation
  ( BaseValidator,
    askFragments,
    checkUnused,
  )
import Relude

checkUnusedFragments :: SelectionSet RAW -> BaseValidator ()
checkUnusedFragments selectionSet = do
  fragments <- askFragments
  usages <- usedFragments fragments selectionSet
  checkUnused usages fragments

usedFragments :: Fragments RAW -> SelectionSet RAW -> BaseValidator (HashMap FragmentName [Ref FragmentName])
usedFragments fragments = collect . map toEntry . concatMap findAllUses . toList
  where
    toEntry (Ref x y) = (x, [Ref x y])
    findUsesSelectionContent :: SelectionContent RAW -> [Ref FragmentName]
    findUsesSelectionContent (SelectionSet selectionSet) =
      concatMap findAllUses selectionSet
    findUsesSelectionContent SelectionField = []
    findAllUses :: Selection RAW -> [Ref FragmentName]
    findAllUses Selection {selectionContent} =
      findUsesSelectionContent selectionContent
    findAllUses (InlineFragment Fragment {fragmentSelection}) =
      concatMap findAllUses fragmentSelection
    findAllUses (Spread _ Ref {refName, refPosition}) =
      [Ref refName refPosition] <> searchInFragment
      where
        searchInFragment =
          selectOr
            []
            (concatMap findAllUses . fragmentSelection)
            refName
            fragments

checkFragmentPreconditions :: SelectionSet RAW -> BaseValidator ()
checkFragmentPreconditions selection =
  (exploreSpreads >>= cycleChecking (throwError . cannotSpreadWithinItself))
    *> checkUnusedFragments selection

exploreSpreads :: BaseValidator (Graph (Ref FragmentName))
exploreSpreads = fmap exploreFragmentSpreads . toList <$> askFragments

exploreFragmentSpreads :: Fragment RAW -> Edges (Ref FragmentName)
exploreFragmentSpreads Fragment {fragmentName, fragmentSelection, fragmentPosition} =
  (Ref fragmentName fragmentPosition, concatMap scanSpread fragmentSelection)

class ScanSpread a where
  scanSpread :: a -> [Ref FragmentName]

instance ScanSpread (Selection RAW) where
  scanSpread Selection {selectionContent} =
    scanSpread selectionContent
  scanSpread (InlineFragment Fragment {fragmentSelection}) =
    concatMap scanSpread fragmentSelection
  scanSpread (Spread _ Ref {refName, refPosition}) =
    [Ref refName refPosition]

instance ScanSpread (SelectionContent RAW) where
  scanSpread SelectionField = []
  scanSpread (SelectionSet selectionSet) =
    concatMap scanSpread selectionSet
