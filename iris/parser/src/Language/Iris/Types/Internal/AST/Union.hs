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

module Language.Iris.Types.Internal.AST.Union
  ( UnionTypeDefinition,
    UnionMember (..),
  )
where

import Data.Mergeable (NameCollision (..), OrdMap)
import Data.Mergeable.Utils (KeyOf (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
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
import Language.Iris.Types.Internal.AST.Stage
  ( Stage,
  )
import Language.Iris.Types.Internal.AST.TypeCategory
  ( TypeCategory,
  )
import Relude hiding (empty)

data UnionMember (cat :: TypeCategory) (s :: Stage) = UnionMember
  { memberDescription :: Maybe Description,
    memberName :: TypeName,
    membership :: Maybe TypeName,
    memberFields :: FieldsDefinition cat s
  }
  deriving (Show, Lift, Eq)


instance NameCollision GQLError (UnionMember c s) where
  nameCollision UnionMember {memberName} =
    "There can Be only one union variant named "
      <> msg memberName

type UnionTypeDefinition c s = OrdMap TypeName (UnionMember c s)

instance RenderGQL (UnionMember cat s) where
  renderGQL = renderGQL . memberName

instance Msg (UnionMember cat s) where
  msg = msg . memberName

instance KeyOf TypeName (UnionMember cat s) where
  keyOf = memberName
