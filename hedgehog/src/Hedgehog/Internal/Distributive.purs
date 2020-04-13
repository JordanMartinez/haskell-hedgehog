module Hedgehog.Internal.Distributive (
    class MonadTransDistributive, distributeT
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Control.Monad.Morph (class MFunctor, hoist)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Identity.Trans (IdentityT(..), runIdentityT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.RWS.Trans (RWST(..), runRWST, RWSResult(..))
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)

------------------------------------------------------------------------
-- * MonadTransDistributive

class MonadTransDistributive g where
  distributeT :: forall f m a
     . Monad m
    => Monad (f m)
    => Monad (g m)
    => Monad (f (g m))
    => MonadTrans f
    => MFunctor f
    => g (f m) a
    -> f (g m) a

instance monadTransDistributiveIdentityT :: MonadTransDistributive IdentityT where
  distributeT m =
    lift <<< IdentityT <<< pure =<< hoist lift (runIdentityT m)

instance monadTransDistributiveMaybeT :: MonadTransDistributive MaybeT where
  distributeT m =
    lift <<< MaybeT <<< pure =<< hoist lift (runMaybeT m)

instance monadTransDistributiveExceptT :: MonadTransDistributive (ExceptT x) where
  distributeT m =
    lift <<< ExceptT <<< pure =<< hoist lift (runExceptT m)

instance monadTransDistributiveReaderT :: MonadTransDistributive (ReaderT r) where
  distributeT m =
    join <<< lift <<< ReaderT $ \r ->
      pure <<< hoist lift $ runReaderT m r

instance monadTransDistributiveWriterT :: Monoid w => MonadTransDistributive (WriterT w) where
  distributeT m = do
    lift <<< WriterT <<< pure =<< hoist lift (runWriterT m)

instance monadTransDistributiveStateT :: MonadTransDistributive (StateT s) where
  distributeT m = do
    s <- lift get
    Tuple a s0 <- hoist lift (runStateT m s)
    lift (put s0)
    pure a

instance monadTransDistributiveRWST :: Monoid w => MonadTransDistributive (RWST r w s) where
  distributeT m = do
    -- ask and get combined
    Tuple r s0 <- lift <<< RWST $ \r s -> pure (RWSResult s (Tuple r s) mempty)
    RWSResult s1 a w <- hoist lift (runRWST m r s0)
    -- tell and put combined
    lift $ RWST $ \_ _ -> pure (RWSResult s1 a w)
