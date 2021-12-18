{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Directive
  ( DirectiveLocation (..),
  )
where

import Language.Haskell.TH.Syntax (Lift)
import Language.Iris.Rendering.RenderGQL
import Language.Iris.Types.Internal.AST.Error (Msg (..))
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
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | DATA_FIELD_DEFINITION
  | SCALAR
  | RESOLVER
  | DATA
  | LIST
  deriving (Show, Eq, Lift)

instance Msg DirectiveLocation where
  msg = msg . show

instance RenderGQL DirectiveLocation where
  renderGQL = fromString . show
