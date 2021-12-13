{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Utils.Result
  ( Result (..),
    resultOr,
    sortErrors,
    toEither,
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.List.NonEmpty as NE
import Data.Text.Lazy.Builder ()
import Relude

--
-- Result
--
--
data Result err a
  = Success {result :: a, warnings :: [err]}
  | Failure {errors :: NonEmpty err}
  deriving (Functor)

instance Applicative (Result er) where
  pure x = Success x []
  Success f w1 <*> Success x w2 = Success (f x) (w1 <> w2)
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure (e :| es) <*> Success _ w = Failure (e :| es <> w)
  Success _ w <*> Failure (e :| es) = Failure (e :| es <> w)

instance Monad (Result er) where
  return = pure
  Success v w1 >>= fm = case fm v of
    (Success x w2) -> Success x (w1 <> w2)
    (Failure (e :| es)) -> Failure (e :| es <> w1)
  Failure e >>= _ = Failure e

instance Bifunctor Result where
  bimap f g Success {..} = Success {warnings = f <$> warnings, result = g result, ..}
  bimap f _ Failure {..} = Failure (f <$> errors)

instance MonadError er (Result er) where
  throwError = Failure . pure
  catchError (Failure (x :| _)) f = f x
  catchError x _ = x

instance IsString err => MonadFail (Result err) where
  fail = Failure . pure . fromString

resultOr :: (NonEmpty err -> a') -> (a -> a') -> Result err a -> a'
resultOr _ f Success {result} = f result
resultOr f _ Failure {errors} = f errors

sortErrors :: Ord e => Result e a -> Result e a
sortErrors (Failure errors) = Failure (NE.sort errors)
sortErrors x = x

toEither :: Result err b -> Either (NonEmpty err) b
toEither = resultOr Left Right
