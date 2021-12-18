{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Name
  ( Name,
    packName,
    unpackName,
    FieldName,
    TypeName,
    unitTypeName,
    isNotSystemTypeName,
    intercalate,
    NAME (..),
    FragmentName,
    __typename,
    unpackVariantTypeName,
    packVariantTypeName,
    isNotSystemName,
    getVariantName,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
  )
import qualified Data.Text as T
import Language.Haskell.TH
  ( Code,
    Quote,
    stringE,
    unsafeCodeCoerce,
  )
import Language.Haskell.TH.Syntax
  ( Lift (..),
  )
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    fromText,
    renderGQL,
  )
import Language.Iris.Types.Internal.AST.Error
  ( Msg (..),
  )
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

data NAME
  = TYPE
  | FIELD
  | FRAGMENT

newtype Name (t :: NAME) = Name {unpackName :: Text}
  deriving
    (Generic)
  deriving newtype
    ( Show,
      Ord,
      Eq,
      IsString,
      ToString,
      Hashable,
      Semigroup,
      FromJSON,
      ToJSON
    )

instance Msg (Name t) where
  msg name = msg $ "\"" <> unpackName name <> "\""

packName :: Text -> Name t
packName = Name

instance Lift (Name t) where
  lift = stringE . T.unpack . unpackName

  liftTyped = liftTypedString . unpackName
    where
      liftTypedString :: (Quote m) => Text -> Code m (Name t)
      liftTypedString = unsafeCodeCoerce . stringE . T.unpack
      {-# INLINE liftTypedString #-}

instance RenderGQL (Name a) where
  renderGQL = fromText . unpackName

type FieldName = Name 'FIELD

type TypeName = Name 'TYPE

type FragmentName = Name 'FRAGMENT

intercalate :: Name t1 -> [Name t2] -> Name t3
intercalate (Name x) = Name . T.intercalate x . fmap unpackName
{-# INLINE intercalate #-}

unitTypeName :: TypeName
unitTypeName = "Unit"
{-# INLINE unitTypeName #-}

isNotSystemName :: Name t -> Bool
isNotSystemName = not . T.isPrefixOf "__" . unpackName

isNotSystemTypeName :: TypeName -> Bool
isNotSystemTypeName name =
  isNotSystemName name
    && ( name
           `notElem` [ "String",
                       "Float",
                       "Int",
                       "Boolean",
                       "ID"
                     ]
       )
{-# INLINE isNotSystemTypeName #-}

__typename :: FieldName
__typename = "__typename"

unpackVariantTypeName :: TypeName -> (TypeName, Maybe TypeName)
unpackVariantTypeName = bimap packName (fmap packName . T.stripPrefix ".") . T.breakOn "." . unpackName

packVariantTypeName :: TypeName -> TypeName -> TypeName
packVariantTypeName typename variantName = typename <> "." <> variantName

getVariantName :: TypeName -> Maybe TypeName
getVariantName = snd . unpackVariantTypeName
