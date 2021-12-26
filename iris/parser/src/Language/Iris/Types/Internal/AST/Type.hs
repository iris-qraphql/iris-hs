{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Type
  ( TypeRef (..),
    Subtyping (..),
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
    renderSeries,
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

-- If S is a subtype of T, "S <: T"
-- A is a subtype of B, then all terms of type A also have type B.
-- type A = { T, Null}
-- type B = T
-- type B is subtype of A since :  {T} ⊂ {T, Null}
-- type B is Subtype if B since: {T} ⊂ {T}
class Subtyping t where
  isSubtype :: t -> t -> Bool

data TypeRef = TypeRef
  { typeRefName :: !TypeName,
    typeParameters :: ![TypeRef],
    isRequired :: !Bool
  }
  deriving (Show, Eq, Lift)

instance Subtyping TypeRef where
  isSubtype (TypeRef name [a] nonNull1) (TypeRef name' [a'] nonNull2) =
    name == name' && nonNull1 >= nonNull2 && isSubtype a' a
  isSubtype (TypeRef name [] a) (TypeRef name' [] b) = name == name' && a >= b
  isSubtype b a = b == a

instance RenderGQL TypeRef where
  renderGQL (TypeRef n xs isNonNull) = renderContent n xs <> renderNonNull isNonNull
    where
      renderContent :: TypeName -> [TypeRef] -> Rendering
      renderContent "List" [x] = "[" <> renderGQL x <> "]"
      renderContent name [] = renderGQL name
      renderContent name pars = renderGQL name <> "<" <> renderSeries pars <> ">"

renderNonNull :: Bool -> Rendering
renderNonNull True = ""
renderNonNull False = "?"

instance Msg TypeRef where
  msg = msg . packName . LT.toStrict . decodeUtf8 . render
