{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.TypeSystem
  ( ScalarDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeDefinitions,
    RawTypeDefinition (..),
    HistoryT,
    startHistory,
    toLocation,
    (<:>),
    mergeNonEmpty,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Mergeable
  ( Merge (..),
    NameCollision (..),
    mergeConcat,
  )
import Data.Mergeable.SafeHashMap
  ( SafeHashMap,
  )
import Data.Mergeable.Utils
  ( KeyOf (..),
  )
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    newline,
  )
import Language.Iris.Types.Internal.AST.Base
  ( Description,
    Ref,
  )
import Language.Iris.Types.Internal.AST.Directive
  ( DirectiveLocation (..),
  )
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    msg,
  )
import Language.Iris.Types.Internal.AST.Fields
  ( DirectiveDefinition (..),
    Directives,
  )
import Language.Iris.Types.Internal.AST.Name
  ( FieldName,
    TypeName,
  )
import Language.Iris.Types.Internal.AST.Role
  ( DATA_TYPE,
    RESOLVER_TYPE,
    Role,
    ToDATA (..),
    ToRESOLVER (..),
    toDATA,
    toRESOLVER,
  )
import Language.Iris.Types.Internal.AST.Stage
  ( CONST,
    Stage,
    VALID,
  )
import Language.Iris.Types.Internal.AST.Value
  ( Value (..),
  )
import Language.Iris.Types.Internal.AST.Variant
  ( Variants,
    renderVariants,
  )
import Relude hiding
  ( empty,
    intercalate,
    show,
  )
import Prelude (Show (..))

mergeNonEmpty :: (Merge (HistoryT m) a, MonadError e m) => NonEmpty a -> m a
mergeNonEmpty = startHistory . mergeConcat

startHistory :: HistoryT m a -> m a
startHistory m = runReaderT m []

type HistoryT = ReaderT [Ref FieldName]

(<:>) :: (Merge (HistoryT m) a, Monad m) => a -> a -> m a
x <:> y = startHistory (merge x y)

newtype ScalarDefinition = ScalarDefinition
  {validateValue :: Value VALID -> Either Text (Value VALID)}

instance Eq ScalarDefinition where
  _ == _ = False

instance Show ScalarDefinition where
  show _ = "ScalarDefinition"

instance Lift ScalarDefinition where
  lift _ = [|ScalarDefinition pure|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped _ = [||ScalarDefinition pure||]
#endif

data RawTypeDefinition
  = RawTypeDefinition (TypeDefinition RESOLVER_TYPE CONST)
  | RawDirectiveDefinition (DirectiveDefinition CONST)
  deriving (Show)

type TypeDefinitions s = SafeHashMap TypeName (TypeDefinition RESOLVER_TYPE s)

data TypeDefinition (a :: Role) (s :: Stage) = TypeDefinition
  { typeDescription :: Maybe Description,
    typeName :: TypeName,
    typeDirectives :: Directives s,
    typeContent :: TypeContent a s
  }
  deriving (Show, Lift, Eq)

instance Ord (TypeDefinition k s) where
  compare a b =
    compare (indexOf $ typeContent a) (indexOf $ typeContent b)
      <> compare (typeName a) (typeName b)

instance KeyOf TypeName (TypeDefinition a s) where
  keyOf = typeName

instance NameCollision GQLError (TypeDefinition cat s) where
  nameCollision x =
    "There can Be only One TypeDefinition Named " <> msg (typeName x) <> "."

instance ToRESOLVER TypeDefinition where
  toRESOLVER TypeDefinition {typeContent, ..} = TypeDefinition {typeContent = toRESOLVER typeContent, ..}

instance ToDATA TypeDefinition where
  toDATA TypeDefinition {typeContent, ..} =
    catchError (to <$> toDATA typeContent) addTypeName
    where
      addTypeName e = throwError (e <> "Type " <> msg typeName <> " ")
      to c = TypeDefinition {typeContent = c, ..}

data
  TypeContent
    (a :: Role)
    (s :: Stage)
  where
  ScalarTypeContent :: {dataScalar :: ScalarDefinition} -> TypeContent a s
  DataTypeContent :: {dataVariants :: Variants DATA_TYPE s} -> TypeContent a s
  ResolverTypeContent ::
    { resolverTypeGuard :: Maybe TypeName,
      resolverVariants :: Variants RESOLVER_TYPE s
    } ->
    TypeContent RESOLVER_TYPE s

deriving instance Show (TypeContent a s)

deriving instance Eq (TypeContent a s)

deriving instance Lift (TypeContent a s)

indexOf :: TypeContent a s -> Int
indexOf ScalarTypeContent {} = 0
indexOf DataTypeContent {} = 1
indexOf ResolverTypeContent {} = 3

instance ToRESOLVER TypeContent where
  toRESOLVER ScalarTypeContent {..} = ScalarTypeContent {..}
  toRESOLVER DataTypeContent {..} = DataTypeContent {..}
  toRESOLVER ResolverTypeContent {..} = ResolverTypeContent {..}

instance ToDATA TypeContent where
  toDATA ScalarTypeContent {..} = pure ScalarTypeContent {..}
  toDATA DataTypeContent {..} = pure DataTypeContent {..}
  toDATA _ = throwError "must be an DATA."

toLocation :: TypeContent a s -> DirectiveLocation
toLocation ScalarTypeContent {} = SCALAR
toLocation DataTypeContent {} = DATA
toLocation ResolverTypeContent {} = RESOLVER

instance RenderGQL (TypeDefinition a s) where
  renderGQL TypeDefinition {typeName, typeContent} = __render typeContent <> newline
    where
      __render ScalarTypeContent {} = "scalar " <> renderGQL typeName
      __render DataTypeContent {dataVariants} = "data " <> renderGQL typeName <> renderVariants typeName dataVariants
      __render ResolverTypeContent {resolverVariants} = "resolver " <> renderGQL typeName <> renderVariants typeName resolverVariants
