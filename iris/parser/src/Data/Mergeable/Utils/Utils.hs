{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Utils.Utils
  ( singleton,
    IsMap,
    KeyOf (..),
    toPair,
    selectBy,
    prop,
    fromLBS,
    toLBS,
    mergeT,
    Empty (..),
    addPath,
    selectOr,
    member,
    unsafeFromList,
    insert,
    fromElems,
    throwErrors,
    fromList,
 
  )
where

import Control.Monad.Except (MonadError)
import Data.ByteString.Lazy (ByteString)
import Data.Mergeable
  ( IsMap,
    Merge (merge),
    NameCollision (..),
    ResolutionT,
    fromListT,
    throwErrors,
  )
import Data.Mergeable.IsMap (FromList (..), member, selectBy, selectOr, unsafeFromList)
import qualified Data.Mergeable.IsMap as M
import Data.Mergeable.SafeHashMap (SafeHashMap)
import Data.Mergeable.Utils.Empty
import Data.Mergeable.Utils.KeyOf (KeyOf (..), toPair)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Instances.TH.Lift ()
import Relude hiding
  ( ByteString,
    decodeUtf8,
    encodeUtf8,
    fromList,
  )

addPath :: MonadReader [entry] m => entry -> m a2 -> m a2
addPath p = local (\xs -> xs <> [p])

toLBS :: Text -> ByteString
toLBS = encodeUtf8 . LT.fromStrict

fromLBS :: ByteString -> Text
fromLBS = LT.toStrict . decodeUtf8

prop :: (b -> b -> m b) -> (a -> b) -> a -> a -> m b
prop f fSel a1 a2 = f (fSel a1) (fSel a2)

singleton :: (IsMap k m, KeyOf k a) => a -> m a
singleton x = M.singleton (keyOf x) x

fromElems ::
  ( Monad m,
    KeyOf k a,
    FromList m map k a
  ) =>
  [a] ->
  m (map k a)
fromElems = fromList . map toPair

insert ::
  ( NameCollision e a,
    KeyOf k a,
    MonadError e m
  ) =>
  a ->
  SafeHashMap k a ->
  m (SafeHashMap k a)
insert x = merge (singleton x)

mergeT :: (KeyOf k a, Foldable t, Monad m) => t a -> t a -> ResolutionT k a c m c
mergeT x y = fromListT (toPair <$> (toList x <> toList y))
