{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.Validation.Scope
  ( Scope (..),
    ScopeKind (..),
    renderScope,
    renderSection,
    setPosition,
    setSelection,
    setDirective,
    setType,
  )
where

import Language.Iris.Rendering.RenderGQL (RenderGQL, render)
import Language.Iris.Types.Internal.AST
  ( Directive (..),
    DirectiveLocation (..),
    FieldName,
    GQLError,
    Msg (msg),
    Position,
    Ref (..),
    TypeDefinition (..),
    toLocation, 
    TypeRef,
  )
import Relude

data ScopeKind
  = DIRECTIVE
  | SELECTION
  | TYPE
  deriving (Show)

data Scope = Scope
  { position :: Maybe Position,
    currentType :: TypeRef,
    currentLocation :: DirectiveLocation,
    fieldName :: FieldName,
    kind :: ScopeKind,
    path :: [Text]
  }
  deriving (Show)

setSelection :: TypeRef -> Ref FieldName -> Scope -> Scope
setSelection t Ref {refName, refPosition} Scope {..} =
  Scope
    { fieldName = refName,
      currentType = t,
      position = Just refPosition,
      ..
    }

setPosition ::
  Position ->
  Scope ->
  Scope
setPosition pos Scope {..} = Scope {position = Just pos, ..}

setDirective :: Directive s -> Scope -> Scope
setDirective Directive {..} Scope {..} =
  Scope
    { fieldName = directiveName,
      position = Just directivePosition,
      kind = DIRECTIVE,
      ..
    }

setType :: TypeDefinition c s -> TypeRef -> Scope -> Scope
setType TypeDefinition {typeContent} ty Scope {..} =
  Scope
    { currentType = ty,
      currentLocation = toLocation typeContent,
      ..
    }

renderScope :: Scope -> GQLError
renderScope
  Scope
    { currentType,
      currentLocation,
      fieldName
    } =
    renderSection
      "Scope"
      ( "referenced by type "
          <> render currentType
          <> " on location "
          <> render currentLocation
          <> " in field "
          <> render fieldName
      )

renderSection :: RenderGQL a => GQLError -> a -> GQLError
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
