-- | Mostly for compatibility across different base Prelude changes.
--
module Hedgehog.Internal.Prelude (
    Semigroup(..)
  , MonadFail
  , module Prelude
  ) where

import           Control.Monad.Fail (MonadFail)

import           Data.Semigroup (Semigroup(..))

import           Prelude hiding (filter, print, map)
