module Hedgehog.Internal.HTraversable
  ( class HTraversable
  , htraverse  
  ) where

import Prelude

-- | Higher-order traversable functors.
--
-- This is used internally to make symbolic variables concrete given an 'Environment'.
--
class HTraversable t where
  htraverse :: forall f g h. Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)
