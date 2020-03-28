module Hedgehog.Internal.Queue (
    TaskIndex(..)
  , TasksRemaining(..)

  , runTasks
  , finalizeTask

  , runActiveFinalizers
  , dequeueMVar

  , updateNumCapabilities
  ) where

import           Control.Concurrent (rtsSupportsBoundThreads)
import           Control.Concurrent.Async (forConcurrently)
import           Control.Concurrent.MVar (MVar)
-- import qualified Control.Concurrent.MVar as MVar
import           Control.Monad (when)
import           Effect.Aff.Class (MonadAff(..))

import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

-- import qualified GHC.Conc as Conc

import           Hedgehog.Internal.Config


newtype TaskIndex = TaskIndex Int
derive newtype instance eqTaskIndex :: Eq TaskIndex
derive newtype instance ordTaskIndex :: Ord TaskIndex
derive newtype instance enumTaskIndex :: Enum TaskIndex
-- TODO: Num instance conversion

newtype TasksRemaining = TasksRemaining Int

dequeueMVar :: forall a b.
     AVar (List (Tuple TaskIndex a))
  -> (TasksRemaining -> TaskIndex -> a -> Aff b)
  -> Aff (Maybe (Tuple TaskIndex b))
dequeueMVar mvar start =
  MVar.modifyMVar mvar $ case _ of
    Nil ->
      pure (Tuple Nil Nothing)
    Cons (Tuple ix x) xs -> do
      y <- start (TasksRemaining $ length xs) ix x
      pure (Tuple xs (Just (Tuple ix y)))

runTasks :: forall a b c.
     WorkerCount
  -> List a
  -> (TasksRemaining -> TaskIndex -> a -> Aff b)
  -> (b -> Aff Unit)
  -> (b -> Aff Unit)
  -> (b -> Aff c)
  -> Aff (List c)
runTasks n tasks start finish finalize runTask = do
  qvar <- MVar.newMVar (zip [0..] tasks)
  fvar <- MVar.newMVar (Tuple -1 Map.empty)

  let
    worker rs = do
      mx <- dequeueMVar qvar start
      case mx of
        Nothing ->
          pure rs
        Just (Tuple ix x) -> do
          r <- runTask x
          finish x
          finalizeTask fvar ix (finalize x)
          worker (r : rs)

  -- FIXME ensure all workers have finished running
  fmap concat <<< forConcurrently [1..max 1 n] $ \_ix ->
    worker []

runActiveFinalizers :: forall m.
     MonadAff m
  => MVar (TaskIndex, Map TaskIndex (Aff ()))
  -> m ()
runActiveFinalizers mvar =
  liftEffect $ do
    again <-
      MVar.modifyMVar mvar $ \original@(Tuple minIx finalizers0) ->
        case Map.minViewWithKey finalizers0 of
          Nothing ->
            pure (Tuple original False)

          Just (Tuple (Tuple ix finalize) finalizers) ->
            if ix == minIx + 1 then do
              finalize
              pure (Tuple (Tuple ix finalizers) True)
            else
              pure (Tuple original False)

    when again $
      runActiveFinalizers mvar

finalizeTask ::
     MonadAff m
  => MVar (TaskIndex, Map TaskIndex (Aff ()))
  -> TaskIndex
  -> Aff ()
  -> m ()
finalizeTask mvar ix finalize = do
  liftEffect <<< MVar.modifyMVar_ mvar $ \(Tuple minIx finalizers) ->
    pure (Tuple minIx (Map.insert ix finalize finalizers))
  runActiveFinalizers mvar

-- | Update the number of capabilities but never set it lower than it already
--   is.
--
updateNumCapabilities :: WorkerCount -> Aff ()
updateNumCapabilities (WorkerCount n) = when rtsSupportsBoundThreads $ do
  ncaps <- Conc.getNumCapabilities
  Conc.setNumCapabilities (max n ncaps)
