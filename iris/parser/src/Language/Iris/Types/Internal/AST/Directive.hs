{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Directive
  ( DirectiveLocation (..),
  )
where

import Language.Haskell.TH.Syntax (Lift)
import Language.Iris.Rendering.RenderGQL (render)
import Language.Iris.Types.Internal.AST.Error (Msg (..))
import Language.Iris.Types.Internal.AST.Type (TypeKind)
import Relude hiding (Show, show)
import Prelude (Show (..))

data DirectiveLocation
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
  | SCHEMA
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | DATA_FIELD_DEFINITION
  | TYPE_DIRECTIVE TypeKind
  deriving (Show, Eq, Lift)

instance Msg DirectiveLocation where
  msg (TYPE_DIRECTIVE x) = msg (render x)
  msg x = msg (show x)

