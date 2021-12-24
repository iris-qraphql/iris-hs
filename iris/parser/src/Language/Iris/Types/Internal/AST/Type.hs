{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Type
  ( TypeRef (..),
    TypeWrapper (..),
    Nullable (..),
    Subtyping (..),
    mkBaseType,
    mkMaybeType,
  )
where

import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    render,
    renderGQL,
  )
import Language.Iris.Types.Internal.AST.Error
  ( Msg (..),
  )
import Language.Iris.Types.Internal.AST.Name
  ( TypeName,
    packName,
  )
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

data TypeWrapper
  = TypeList
      { wrapperName :: TypeName,
        wrapperParameter :: !TypeWrapper,
        listIsRequired :: !Bool
      }
  | BaseType {isBaseTypeRequired :: !Bool}
  deriving (Show, Eq, Lift)

mkBaseType :: TypeWrapper
mkBaseType = BaseType True

mkMaybeType :: TypeWrapper
mkMaybeType = BaseType False

-- If S is a subtype of T, "S <: T"
-- A is a subtype of B, then all terms of type A also have type B.
-- type B = Int | Null
-- type A = Int
-- A <: B
--
-- interface A { a: String }
--
-- type B implements A { a: String!}
--
-- type B is subtype of A since :  {String} ⊂ {String, null}
--
-- interface A { a: String }
--
-- type B implements A { a: String }
--
-- type B is not subtype of A since :  {String, null} ⊂ {String}
--
-- type A = { T, Null}
-- type B = T
-- type B is subtype of A since :  {T} ⊂ {T, Null}
-- type B is Subtype if B since: {T} ⊂ {T}
class Subtyping t where
  isSubtype :: t -> t -> Bool

instance Subtyping TypeWrapper where
  isSubtype (TypeList name a nonNull1) (TypeList name' a' nonNull2) =
    name == name' && nonNull1 >= nonNull2 && isSubtype a' a
  isSubtype (BaseType b) (BaseType a) = b >= a
  isSubtype b a = b == a

-- TypeRef
-------------------------------------------------------------------
data TypeRef = TypeRef
  { typeConName :: TypeName,
    typeWrappers :: TypeWrapper
  }
  deriving (Show, Eq, Lift)

instance Subtyping TypeRef where
  isSubtype t1 t2 =
    typeConName t1 == typeConName t2
      && typeWrappers t1 `isSubtype` typeWrappers t2

instance RenderGQL TypeRef where
  renderGQL TypeRef {typeConName, typeWrappers} = renderWrapper typeWrappers
    where
      renderWrapper (TypeList name xs isNonNull) = renderContent name xs <> renderNonNull isNonNull
      renderWrapper (BaseType isNonNull) = renderGQL typeConName <> renderNonNull isNonNull
      --
      renderContent "List" xs = "[" <> renderWrapper xs <> "]"
      renderContent name xs = renderGQL name <> "<" <> renderWrapper xs <> ">"

renderNonNull :: Bool -> Rendering
renderNonNull True = ""
renderNonNull False = "?"

instance Msg TypeRef where
  msg = msg . packName . LT.toStrict . decodeUtf8 . render

class Nullable a where
  isNullable :: a -> Bool

instance Nullable TypeWrapper where
  isNullable TypeList {listIsRequired} = not listIsRequired
  isNullable (BaseType nonNull) = not nonNull

instance Nullable TypeRef where
  isNullable = isNullable . typeWrappers
