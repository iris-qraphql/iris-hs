{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Variant
  ( Variants,
    Variant (..),
    renderVariants,
    lookupTypeVariant,
    variantTypeName,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Mergeable (NameCollision (..))
import Data.Mergeable.Utils (KeyOf (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    intercalate,
    space,
  )
import Language.Iris.Types.Internal.AST.Base (Description)
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    Msg (..),
    msg,
  )
import Language.Iris.Types.Internal.AST.Fields (FieldsDefinition)
import Language.Iris.Types.Internal.AST.Name
  ( TypeName,
  )
import Language.Iris.Types.Internal.AST.Role
  ( Role,
  )
import Language.Iris.Types.Internal.AST.Stage
  ( Stage,
  )
import Relude hiding (empty, intercalate)

type Variants c s = NonEmpty (Variant c s)

variantTypeName :: Variant a s -> TypeName
variantTypeName Variant {membership, variantName} = maybe variantName (<> "." <> variantName) membership

noVariant :: MonadError GQLError m => TypeName -> m a
noVariant name = throwError $ "Can't find variant " <> msg name <> "."

lookupTypeVariant :: MonadError GQLError m => Maybe TypeName -> Variants cat s -> m (Variant cat s)
lookupTypeVariant (Just name) variants = maybe (noVariant name) pure (find ((name ==) . variantName) variants)
lookupTypeVariant Nothing (x :| []) = pure x
lookupTypeVariant Nothing _ = throwError "type should have only one variant"

renderVariants :: TypeName -> NonEmpty (Variant cat s) -> Rendering
renderVariants typeName (Variant {variantFields, variantName} :| []) | typeName == variantName = " =" <> renderGQL variantFields
renderVariants _ variants =
  " = "
    <> intercalate
      (space <> "|" <> space)
      (fmap renderGQL (toList variants))

data Variant (r :: Role) (stage :: Stage) = Variant
  { variantDescription :: Maybe Description,
    variantName :: TypeName,
    membership :: Maybe TypeName,
    variantFields :: FieldsDefinition r stage
  }
  deriving (Show, Lift, Eq)

instance NameCollision GQLError (Variant c s) where
  nameCollision Variant {variantName} =
    "There can Be only one union variant named "
      <> msg variantName

instance RenderGQL (Variant cat s) where
  renderGQL Variant {variantName, variantFields} =
    renderGQL variantName <> renderGQL variantFields

instance Msg (Variant cat s) where
  msg = msg . variantName

instance KeyOf TypeName (Variant cat s) where
  keyOf = variantName
