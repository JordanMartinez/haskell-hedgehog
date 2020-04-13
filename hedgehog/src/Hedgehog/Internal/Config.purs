module Hedgehog.Internal.Config (
    UseColor(..)
  , resolveColor

  , Verbosity(..)
  , resolveVerbosity

  , WorkerCount(..)
  , resolveWorkers

  , detectMark
  , detectColor
  , detectVerbosity
  -- , detectWorkers -- see Port Notes below
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Node.Process (lookupEnv)
import Data.Int (fromString)
import Data.Enum (class Enum)

-- | Whether to render output using ANSI colors or not.
-- | DisableColor = Disable ANSI colors in report output.
-- | EnableColor = Enable ANSI colors in report output.
data UseColor =
    DisableColor
  | EnableColor

derive instance eqUseColor :: Eq UseColor
derive instance ordUseColor :: Ord UseColor
derive instance genericUseColor :: Generic UseColor _
instance showUseColor :: Show UseColor where
  show x = genericShow x

-- | How verbose should the report output be.
-- | Quiet = Only display the summary of the test run.
-- | Normal = Display each property as it is running, as well as the summary.
data Verbosity =
    Quiet
  | Normal

derive instance eqVerbosity :: Eq Verbosity
derive instance ordVerbosity :: Ord Verbosity
derive instance genericVerbosity :: Generic Verbosity _
instance showVerbosity :: Show Verbosity where
  show x =
    genericShow x

-- | The number of workers to use when running properties in parallel.
newtype WorkerCount = WorkerCount Int

derive instance eqWorkerCount :: Eq WorkerCount
derive instance ordWorkerCount :: Ord WorkerCount
derive instance genericWorkerCount :: Generic WorkerCount _
instance showWorkerCount :: Show WorkerCount where
  show x =
    genericShow x
derive newtype instance enumWorkerCount :: Enum WorkerCount
instance semigroupWorkerCount :: Semigroup WorkerCount where
  append (WorkerCount x) (WorkerCount y) =
    WorkerCount (x + y)
instance monoidWorkerCount :: Monoid WorkerCount where
  mempty =
    WorkerCount 0
derive newtype instance semiringWorkerCount :: Semiring WorkerCount

detectMark :: forall m. MonadEffect m => m Boolean
detectMark = do
  user <- liftEffect $ lookupEnv "USER"
  pure $ user == Just "mth"

lookupBoolean :: forall m. MonadEffect m => String -> m (Maybe Boolean)
lookupBoolean key =
  liftEffect do
    menv <- lookupEnv key
    case menv of
      Just "0" ->
        pure $ Just false
      Just "no" ->
        pure $ Just false
      Just "false" ->
        pure $ Just false

      Just "1" ->
        pure $ Just true
      Just "yes" ->
        pure $ Just true
      Just "true" ->
        pure $ Just true

      _ ->
        pure Nothing

detectColor :: forall m. MonadEffect m => m UseColor
detectColor =
  liftEffect $ do
    ok <- lookupBoolean "HEDGEHOG_COLOR"
    case ok of
      Just false ->
        pure DisableColor

      Just true ->
        pure EnableColor

      Nothing -> do
        mth <- detectMark
        if mth then
          pure DisableColor -- avoid getting fired :)
        else
          -- Note: Didn't find anything when googling for how to detect
          -- whether terminal supports ANSI escape codes on Node.js.
          -- I will assume that Node.js and browser always support this
          pure EnableColor

detectVerbosity :: forall m. MonadEffect m => m Verbosity
detectVerbosity =
  liftEffect $ do
    menv <- (fromString =<< _) <$> lookupEnv "HEDGEHOG_VERBOSITY"
    case menv of
      Just 0 ->
        pure Quiet

      Just 1 ->
        pure Normal

      _ -> do
        mth <- detectMark
        if mth then
          pure Quiet
        else
          pure Normal

-- Port Note: Node/Browser is single-threaded. While this port could be written
-- to utilize worker threads, that's not my current focus right now
-- in this initial port. Similarly, other PureScript backends that could
-- support multi-threaded environments are not in focus as I make this
-- initial port.
--
-- detectWorkers :: forall m. MonadEffect m => m WorkerCount
-- detectWorkers = do
--   liftEffect $ do
--     menv <- fromString <$> lookupEnv "HEDGEHOG_WORKERS"
--     case menv of
--       Nothing ->
--         WorkerCount <$> Conc.getNumProcessors
--       Just env ->
--         pure $ WorkerCount env

resolveColor :: forall m. MonadEffect m => Maybe UseColor -> m UseColor
resolveColor = case _ of
  Nothing ->
    detectColor
  Just x ->
    pure x

resolveVerbosity :: forall m. MonadEffect m => Maybe Verbosity -> m Verbosity
resolveVerbosity = case _ of
  Nothing ->
    detectVerbosity
  Just x ->
    pure x

resolveWorkers :: forall m. MonadEffect m => Maybe WorkerCount -> m WorkerCount
resolveWorkers = const (pure (WorkerCount 1))
  -- Port Note: reexamine whether this can be achieved via worker threads
  -- case _ of
  -- Nothing ->
  --   detectWorkers
  -- Just x ->
  --   pure x
