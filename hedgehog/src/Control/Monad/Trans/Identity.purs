module Control.Monad.Trans.Identity where

import Prelude
import Control.Monad.Trans.Class (class MonadTrans)

newtype IdentityT f a = IdentityT (f a)

runIdentityT :: forall f a. IdentityT f a -> f a
runIdentityT (IdentityT f) = f

derive newtype instance eqIdentityT :: Eq (f a) => Eq (IdentityT f a)
derive newtype instance ordIdentityT :: Ord (f a) => Ord (IdentityT f a)
derive newtype instance functorIdentityT :: Functor f => Functor (IdentityT f)
derive newtype instance applyIdentityT :: Apply f => Apply (IdentityT f)
derive newtype instance applicativeIdentityT :: Applicative f => Applicative (IdentityT f)
derive newtype instance bindIdentityT :: Bind f => Bind (IdentityT f)
derive newtype instance monadIdentityT :: Monad f => Monad (IdentityT f)
instance monadTransIdentityT :: MonadTrans IdentityT where
  lift :: forall m a. Monad m => m a -> IdentityT m a
  lift = IdentityT
