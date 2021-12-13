{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Query.UnionSelection
  ( validateUnionSelection,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable (OrdMap)
import Data.Mergeable.Utils
  ( empty,
    fromElems,
    selectOr,
  )
import Language.Iris.Types.Internal.AST (mergeNonEmpty, startHistory)
import Language.Iris.Types.Internal.AST.Base (Position (..))
import Language.Iris.Types.Internal.AST.Name (TypeName)
import Language.Iris.Types.Internal.AST.Selection
  ( Fragment (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    UnionTag (..),
  )
import Language.Iris.Types.Internal.AST.Stage (RAW, VALID)
import Language.Iris.Types.Internal.AST.TypeCategory
  ( LAZY,
    OBJECT,
  )
import Language.Iris.Types.Internal.AST.TypeSystem
  ( TypeContent (..),
    TypeDefinition (..),
    UnionTypeDefinition,
    mkType,
  )
import Language.Iris.Types.Internal.Validation
  ( FragmentValidator,
    Scope (..),
    askObjectType,
    askTypeMember,
    asksScope,
  )
import Language.Iris.Validation.Query.Fragment
  ( ValidateFragmentSelection,
    castFragmentType,
    validateSpread,
  )
import Relude hiding (empty, join)

-- returns all Fragments used for Possible Types
splitFragment ::
  (ValidateFragmentSelection s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition OBJECT VALID] ->
  Selection RAW ->
  FragmentValidator s (Either UnionTag (Selection RAW))
splitFragment _ _ x@Selection {} = pure (Right x)
splitFragment f types (Spread _ ref) = Left <$> validateSpread f (typeName <$> types) ref
splitFragment f types (InlineFragment fragment@Fragment {fragmentType}) =
  Left . UnionTag fragmentType
    <$> ( castFragmentType Nothing (fragmentPosition fragment) (typeName <$> types) fragment
            >>= f
        )

exploreFragments ::
  (ValidateFragmentSelection s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition OBJECT VALID] ->
  SelectionSet RAW ->
  FragmentValidator s ([UnionTag], SelectionSet RAW)
exploreFragments validateFragment types selectionSet = do
  (tags, selections) <- partitionEithers <$> traverse (splitFragment validateFragment (toList types)) (toList selectionSet)
  selectionPosition <- fromMaybe (Position 0 0) <$> asksScope position
  (tags,)
    <$> fromElems
      ( ( Selection
            { selectionName = "__typename",
              selectionAlias = Nothing,
              selectionPosition,
              selectionArguments = empty,
              selectionContent = SelectionField,
              selectionDirectives = empty
            }
        )
          : selections
      )

-- sorts Fragment by conditional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [UnionTag] ->
  OrdMap TypeName (TypeDefinition OBJECT VALID) ->
  HashMap TypeName [SelectionSet VALID]
tagUnionFragments fragments types = fmap categorizeType getSelectedTypes
  where
    getSelectedTypes :: HashMap TypeName [TypeName]
    getSelectedTypes = fromList (map select fragments)
      where
        select UnionTag {unionTagName} =
          ( unionTagName,
            selectOr
              [unionTagName]
              getCompatibleTypes
              unionTagName
              types
          )
    categorizeType ::
      [TypeName] -> [SelectionSet VALID]
    categorizeType compatibleTypes =
      unionTagSelection
        <$> filter
          ((`elem` compatibleTypes) . unionTagName)
          fragments

-- TODO: replace with type Guards
getCompatibleTypes :: TypeDefinition a s -> [TypeName]
getCompatibleTypes TypeDefinition {typeName} = [typeName]

joinClusters ::
  SelectionSet VALID ->
  HashMap TypeName [SelectionSet VALID] ->
  FragmentValidator s (SelectionContent VALID)
joinClusters selSet typedSelections
  | null typedSelections = pure (SelectionSet selSet)
  | otherwise =
    traverse mkUnionTag (HM.toList typedSelections)
      >>= fmap (UnionSelection selSet) . startHistory . fromElems
  where
    mkUnionTag :: (TypeName, [SelectionSet VALID]) -> FragmentValidator s UnionTag
    mkUnionTag (typeName, fragments) = UnionTag typeName <$> mergeNonEmpty (selSet :| fragments)

mkUnionRootType :: FragmentValidator s (TypeDefinition OBJECT VALID)
mkUnionRootType = (`mkType` LazyTypeContent empty) <$> asksScope currentTypeName

validateUnionSelection ::
  ValidateFragmentSelection s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition OBJECT VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  Maybe TypeName ->
  UnionTypeDefinition LAZY VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validateFragment validate guardName members inputSelectionSet = do
  unionTypes <- traverse askTypeMember members
  x <- traverse askObjectType (toList guardName)
  (spreads, selectionSet) <- exploreFragments validateFragment (x <> toList unionTypes) inputSelectionSet
  typeDef <- maybe mkUnionRootType askObjectType guardName
  validSelectionSet <- validate typeDef selectionSet
  let tags = tagUnionFragments spreads unionTypes
  let typeGuardFragments = maybe [] (\name -> selectOr [] id name tags) guardName
  defaultSelection <- mergeNonEmpty (validSelectionSet :| typeGuardFragments)
  joinClusters defaultSelection (maybe tags (`HM.delete` tags) guardName)
