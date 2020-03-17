module Hedgehog.Internal.Distributive (
    class MonadTransDistributive, distributeT
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Control.Monad.Morph (class MFunctor, hoist)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Trans.Identity (IdentityT(..), runIdentityT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
-- import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import Control.Monad.RWS.Trans (RWST(..), runRWST, RWSResult(..))
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
-- import qualified Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Trans (StateT, runStateT)
-- import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Writer.Trans (WriterT(..), runWriterT)

-- import GHC.Exts (Constraint)

------------------------------------------------------------------------
-- * MonadTransDistributive

class MonadTransDistributive g where
  -- type Transformer
  --   (f :: (* -> *) -> * -> *)
  --   (g :: (* -> *) -> * -> *)
  --   (m :: * -> *) :: Constraint
  --
  -- type Transformer f g m = (
  --     Monad m
  --   , Monad (f m)
  --   , Monad (g m)
  --   , Monad (f (g m))
  --   , MonadTrans f
  --   , MFunctor f
  --   )

  -- | Distribute one monad transformer over another.
  distributeT :: forall f m a.
    Monad m => Monad (f m) => Monad (g m) => Monad (f (g m)) => MonadTrans f => MFunctor f =>
    g (f m) a -> f (g m) a

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

-- instance Monoid w => MonadTransDistributive (Lazy.WriterT w) where
--   distributeT m =
--     lift <<< Lazy.WriterT <<< pure =<< hoist lift (Lazy.runWriterT m)

instance monadTransDistributiveWriterT :: Monoid w => MonadTransDistributive (WriterT w) where
  distributeT m = do
    lift <<< WriterT <<< pure =<< hoist lift (runWriterT m)

-- instance MonadTransDistributive (Lazy.StateT s) where
--   distributeT m = do
--     s       <- lift Lazy.get
--     (a, s') <- hoist lift (Lazy.runStateT m s)
--     lift (Lazy.put s')
--     return a

instance monadTransDistributiveStateT :: MonadTransDistributive (StateT s) where
  distributeT m = do
    s       <- lift get
    Tuple a s' <- hoist lift (runStateT m s)
    lift (put s')
    pure a

-- instance Monoid w => MonadTransDistributive (Lazy.RWST r w s) where
--   distributeT m = do
--     -- ask and get combined
--     (r, s0)    <- lift <<< Lazy.RWST $ \r s -> return ((r, s), s, mempty)
--     (a, s1, w) <- hoist lift (Lazy.runRWST m r s0)
--     -- tell and put combined
--     lift $ Lazy.RWST $ \_ _ -> return (a, s1, w)

instance monadTransDistributiveRWST :: Monoid w => MonadTransDistributive (RWST r w s) where
  distributeT m = do
    -- ask and get combined
    Tuple r s0 <- lift <<< RWST $ \r s -> pure (RWSResult s (Tuple r s) mempty)
    RWSResult s1 a w <- hoist lift (runRWST m r s0)
    -- tell and put combined
    lift $ RWST $ \_ _ -> pure (RWSResult s1 a w)
