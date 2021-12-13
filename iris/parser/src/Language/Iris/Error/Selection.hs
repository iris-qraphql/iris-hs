{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Error.Selection
  ( subfieldsNotSelected,
    hasNoSubfields,
    cannotBeSpreadOnType,
    mutationIsNotDefined,
    subscriptionIsNotDefined,
    cannotSpreadWithinItself,
    deprecatedField,
  )
where

import Language.Iris.Types.Internal.AST.Base
  ( Description,
    Position,
    Ref (..),
  )
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    Msg,
    at,
    atPositions,
    manyMsg,
    msg,
  )
import Language.Iris.Types.Internal.AST.Name
  ( FieldName,
    FragmentName,
    TypeName,
  )
import Relude

-- Operation

mutationIsNotDefined :: Position -> GQLError
mutationIsNotDefined position =
  "Schema is not configured for mutations." `at` position

subscriptionIsNotDefined :: Position -> GQLError
subscriptionIsNotDefined position =
  "Schema is not configured for subscriptions." `at` position

-- Fields
hasNoSubfields :: Ref FieldName -> TypeName -> GQLError
hasNoSubfields (Ref selectionName position) typeName = text `at` position
  where
    text =
      "Field "
        <> msg selectionName
        <> " must not have a selection since type "
        <> msg typeName
        <> " has no subfields."

subfieldsNotSelected :: FieldName -> TypeName -> Position -> GQLError
subfieldsNotSelected fieldName typeName position = text `at` position
  where
    text =
      "Field " <> msg fieldName <> " of type "
        <> msg typeName
        <> " must have a selection of subfields"

cannotSpreadWithinItself :: NonEmpty (Ref FragmentName) -> GQLError
cannotSpreadWithinItself (fr :| frs) =
  ( "Cannot spread fragment "
      <> msg (refName fr)
      <> " within itself via "
      <> manyMsg (refName <$> (fr : frs))
      <> "."
  )
    `atPositions` map refPosition (fr : frs)

cannotBeSpreadOnType :: Maybe FragmentName -> TypeName -> Position -> [TypeName] -> GQLError
cannotBeSpreadOnType key fragmentType position typeMembers =
  ( "Fragment "
      <> getName key
      <> "cannot be spread here as objects of type "
      <> manyMsg typeMembers
      <> " can never be of type "
      <> msg fragmentType
      <> "."
  )
    `at` position

getName :: Msg a => Maybe a -> GQLError
getName (Just x) = msg x <> " "
getName Nothing = ""

deprecatedField :: FieldName -> Ref FieldName -> Maybe Description -> GQLError
deprecatedField typeName Ref {refPosition, refName} reason =
  "the field "
    <> msg typeName
    <> "."
    <> msg refName
    <> " is deprecated."
    <> msg (maybe "" (" " <>) reason) `at` refPosition
