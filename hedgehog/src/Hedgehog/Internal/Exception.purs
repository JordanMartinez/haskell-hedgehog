module Hedgehog.Internal.Exception (
    tryAll
  , tryEvaluate
  ) where

import Prelude
import           Control.Exception (Exception(..), AsyncException, SomeException(..), evaluate)
import           Control.Monad.Catch (MonadCatch(..), throwM)

import           System.IO.Unsafe (unsafePerformIO)


tryAll :: forall e m a. MonadError e m => m a -> m (Either SomeException a)
tryAll m =
  catchError (map Right m) $ \exception ->
    case fromException exception :: Maybe AsyncException of
      Nothing ->
        pure $ Left exception
      Just async ->
        throwM async

-- Note: evaluate is used to deal with laziness things
-- This may not be necessary in a strict language
tryEvaluate :: a -> Either SomeException a
tryEvaluate x =
  unsafePartial (unsafePerformEffect (tryAll (evaluate x)))
