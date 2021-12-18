{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Value
  ( Value (..),
    ScalarValue (..),
    Object,
    replaceValue,
    decodeScientific,
    RawValue,
    ValidValue,
    RawObject,
    ValidObject,
    Variable (..),
    ResolvedValue,
    ResolvedObject,
    VariableContent (..),
    ObjectEntry (..),
    VariableDefinitions,
  )
where

import Control.Monad.Except
import qualified Data.Aeson as A
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    pairs,
    (.=),
  )
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Foldable (foldr')
import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( IsMap (unsafeFromList),
    NameCollision (..),
    OrdMap,
  )
import Data.Mergeable.Utils
  ( KeyOf (..),
  )
import Data.Scientific
  ( Scientific,
    floatingOrInteger,
  )
import qualified Data.Text as T
import qualified Data.Vector as V
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    fromText,
    render,
    renderInputSeq,
    space,
  )
import Language.Iris.Types.Internal.AST.Base
  ( Position,
    Ref (..),
  )
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    Msg (..),
    at,
  )
import Language.Iris.Types.Internal.AST.Name
  ( FieldName,
    TypeName,
    packName,
    unpackName,
    __typename,
  )
import Language.Iris.Types.Internal.AST.Stage
  ( CONST,
    CONST_OR_VALID,
    RAW,
    Stage,
    VALID,
  )
import Language.Iris.Types.Internal.AST.Type (TypeRef (..))
import Relude hiding (fromList)

-- | Primitive Values for GQLScalar: 'Int', 'Float', 'String', 'Boolean'.
-- for performance reason type 'Text' represents GraphQl 'String' value
data ScalarValue
  = Int Int
  | Float Double
  | String Text
  | Boolean Bool
  | Value A.Value
  deriving (Show, Eq, Generic, Lift)

instance IsString ScalarValue where
  fromString = String . T.pack

instance RenderGQL ScalarValue where
  renderGQL (Int x) = renderGQL x
  renderGQL (Float x) = renderGQL x
  renderGQL (String x) = renderGQL x
  renderGQL (Boolean x) = renderGQL x
  renderGQL (Value x) = renderGQL x

instance A.ToJSON ScalarValue where
  toJSON (Float x) = A.toJSON x
  toJSON (Int x) = A.toJSON x
  toJSON (Boolean x) = A.toJSON x
  toJSON (String x) = A.toJSON x
  toJSON (Value x) = A.toJSON x

instance A.FromJSON ScalarValue where
  parseJSON (A.Bool v) = pure $ Boolean v
  parseJSON (A.Number v) = pure $ decodeScientific v
  parseJSON (A.String v) = pure $ String v
  parseJSON notScalar = fail $ "Expected Scalar got :" <> show notScalar

data VariableContent (stage :: Stage) where
  DefaultValue :: Maybe ResolvedValue -> VariableContent CONST
  ValidVariableValue :: {validVarContent :: ValidValue} -> VariableContent VALID

instance Lift (VariableContent a) where
  lift (DefaultValue x) = [|DefaultValue x|]
  lift (ValidVariableValue x) = [|ValidVariableValue x|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (DefaultValue x) = [||DefaultValue x||]
  liftTyped (ValidVariableValue x) = [||ValidVariableValue x||]
#endif
deriving instance Show (VariableContent a)

deriving instance Eq (VariableContent a)

data Variable (stage :: Stage) = Variable
  { variablePosition :: Position,
    variableName :: FieldName,
    variableType :: TypeRef,
    variableValue :: VariableContent (CONST_OR_VALID stage)
  }
  deriving (Show, Eq, Lift)

instance KeyOf FieldName (Variable s) where
  keyOf = variableName

instance NameCollision GQLError (Variable s) where
  nameCollision Variable {variableName, variablePosition} =
    ("There can Be only One Variable Named " <> msg variableName)
      `at` variablePosition

type VariableDefinitions s = OrdMap FieldName (Variable s)

data Value (stage :: Stage) where
  ResolvedVariable :: Ref FieldName -> Variable VALID -> Value CONST
  VariableValue :: Ref FieldName -> Value RAW
  Object :: Maybe TypeName -> Object stage -> Value stage
  List :: [Value stage] -> Value stage
  Scalar :: ScalarValue -> Value stage
  Null :: Value stage

instance IsString (Value stage) where
  fromString = Scalar . fromString

deriving instance Show (Value a)

deriving instance Eq (Value s)

data ObjectEntry (s :: Stage) = ObjectEntry
  { entryName :: FieldName,
    entryValue :: Value s
  }
  deriving (Eq, Show)

instance RenderGQL (ObjectEntry a) where
  renderGQL (ObjectEntry name value) = fromText (unpackName name) <> ": " <> renderGQL value

instance NameCollision GQLError (ObjectEntry s) where
  nameCollision ObjectEntry {entryName} =
    "There can Be only One field Named "
      <> msg entryName

instance KeyOf FieldName (ObjectEntry s) where
  keyOf = entryName

type Object a = OrdMap FieldName (ObjectEntry a)

type ValidObject = Object VALID

type RawObject = Object RAW

type ResolvedObject = Object CONST

type RawValue = Value RAW

type ValidValue = Value VALID

type ResolvedValue = Value CONST

deriving instance Lift (Value a)

deriving instance Lift (ObjectEntry a)

instance RenderGQL (Value a) where
  renderGQL (ResolvedVariable Ref {refName} _) = "$" <> renderGQL refName
  renderGQL (VariableValue Ref {refName}) = "$" <> renderGQL refName <> " "
  renderGQL Null = "null"
  renderGQL (Scalar x) = renderGQL x
  renderGQL (Object typeName fields)
    | null typeName = "{" <> entries <> "}"
    | null fields = renderGQL typeName
    | otherwise = renderGQL typeName <> " {" <> entries <> "}"
    where
      entries
        | null fields = ""
        | otherwise = space <> renderInputSeq (toList fields) <> space
  renderGQL (List list) = "[" <> renderInputSeq list <> "]"

instance Msg (Value a) where
  msg = msg . render

instance A.ToJSON (Value a) where
  toJSON (ResolvedVariable _ Variable {variableValue = ValidVariableValue x}) =
    A.toJSON x
  toJSON (VariableValue Ref {refName}) =
    A.String $ "($ref:" <> unpackName refName <> ")"
  toJSON Null = A.Null
  toJSON (Scalar x) = A.toJSON x
  toJSON (List x) = A.toJSON x
  toJSON (Object conName fields) = A.object $ fmap toEntry (typeNameEntry conName <> toList fields)
    where
      toEntry (ObjectEntry name value) = unpackName name A..= A.toJSON value

  -------------------------------------------
  toEncoding (ResolvedVariable _ Variable {variableValue = ValidVariableValue x}) =
    A.toEncoding x
  toEncoding (VariableValue Ref {refName}) =
    A.toEncoding $ "($ref:" <> refName <> ")"
  toEncoding Null = A.toEncoding A.Null
  toEncoding (Scalar x) = A.toEncoding x
  toEncoding (List x) = A.toEncoding x
  toEncoding (Object conName ordMap) = A.pairs $ renderSeries encodeField (typeNameEntry conName <> toList ordMap)
    where
      encodeField (ObjectEntry key value) = unpackName key A..= value

typeNameEntry :: Maybe TypeName -> [ObjectEntry s]
typeNameEntry Nothing = []
typeNameEntry (Just name) = [ObjectEntry __typename (Scalar (String (unpackName name)))]

-- fixes GHC 8.2.2, which can't deduce (Semigroup p) from context (Monoid p)
renderSeries :: (Semigroup p, Monoid p) => (e -> p) -> [e] -> p
renderSeries _ [] = mempty
renderSeries f (x : xs) = foldr' (\e es -> es <> f e) (f x) xs

decodeScientific :: Scientific -> ScalarValue
decodeScientific v = case floatingOrInteger v of
  Left float -> Float float
  Right int -> Int int

replaceValue :: MonadFail m => A.Value -> m (Value a)
replaceValue (A.Bool v) = pure (mkBoolean v)
replaceValue (A.Number v) = pure $ Scalar $ decodeScientific v
replaceValue (A.String v) = pure $ mkString v
replaceValue (A.Object v) = mkObject v
replaceValue (A.Array li) = List <$> traverse replaceValue (V.toList li)
replaceValue A.Null = pure Null

instance A.FromJSON (Value a) where
  parseJSON = replaceValue

mkBoolean :: Bool -> Value s
mkBoolean = Scalar . Boolean

mkString :: Text -> Value s
mkString = Scalar . String

mkObject :: (MonadFail m) => HashMap Text A.Value -> m (Value s)
mkObject hm = Object <$> defaultValue <*> fields
  where
    typename = unpackName __typename
    fields = unsafeFromList <$> traverse toEntry (HM.toList $ HM.delete typename hm)
    defaultValue = traverse unpackTypeName (HM.lookup typename hm)
    toEntry (key, value) = (packName key,) . ObjectEntry (packName key) <$> replaceValue value

unpackTypeName :: MonadFail m => A.Value -> m TypeName
unpackTypeName (A.String name) = pure (packName name)
unpackTypeName v = fail $ "object " <> show __typename <> " must be a String but got " <> LB.unpack (render v) <> "."
