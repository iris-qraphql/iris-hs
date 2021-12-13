{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Iris.App.Internal.Resolving.Result
  ( ResultT (..),
    mapEvent,
    cleanEvents,
    PushEvents (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Mergeable.Utils
  ( Result (..),
  )
import Data.Text.Lazy.Builder ()
import Language.Iris.Types.Internal.AST (GQLError (..))
import Relude

-- EVENTS
class PushEvents e m where
  pushEvents :: [e] -> m ()

-- ResultT
newtype ResultT event (m :: Type -> Type) a = ResultT
  { runResultT :: m (Result GQLError ([event], a))
  }
  deriving (Functor)

instance Applicative m => Applicative (ResultT event m) where
  pure = ResultT . pure . pure . ([],)
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) (fx <$> app1) app2
    where
      fx :: Monad f => f ([event], a -> b) -> f (([event], a) -> ([event], b))
      fx x = do
        (e', f) <- x
        pure $ \(e, a) -> (e <> e', f a)

instance Monad m => Monad (ResultT event m) where
  return = pure
  (ResultT m1) >>= mFunc = ResultT $ do
    res <- m1
    case res of
      Failure err -> pure $ Failure err
      Success (events, value) w1 -> do
        result' <- runResultT (mFunc value)
        case result' of
          Failure (e :| es) -> pure $ Failure (e :| es <> w1)
          Success (events', value') w2 -> pure $ Success (events <> events', value') (w1 <> w2)

instance MonadTrans (ResultT event) where
  lift = ResultT . fmap (pure . ([],))

instance Monad m => MonadError GQLError (ResultT event m) where
  throwError = ResultT . pure . throwError
  catchError (ResultT mx) f = ResultT (mx >>= catchResultError)
    where
      catchResultError (Failure (x :| _)) = runResultT (f x)
      catchResultError x = pure x

instance Applicative m => PushEvents event (ResultT event m) where
  pushEvents x = ResultT $ pure $ pure (x, ())

cleanEvents :: Functor m => ResultT e m a -> ResultT e' m a
cleanEvents resT = ResultT $ fmap (first (const [])) <$> runResultT resT

mapEvent :: Monad m => (e -> e') -> ResultT e m value -> ResultT e' m value
mapEvent func (ResultT ma) = ResultT $ fmap (first (map func)) <$> ma
