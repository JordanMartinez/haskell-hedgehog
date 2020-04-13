module Hedgehog.Internal.Opaque (
    Opaque(..)
  ) where

import Prelude

-- | Opaque values.
-- |
-- | Useful if you want to put something without a 'Show' instance inside
-- | something which you'd like to be able to display.
-- |
-- | For example:
-- |
-- | ```
-- | type State v =
-- |    { stateRefs :: List (Var (Opaque (Ref Int)) v) }
-- | ```
newtype Opaque a =
  Opaque a

derive newtype instance eqOpaque :: Eq a => Eq (Opaque a)
derive newtype instance ordOpaque :: Ord a => Ord (Opaque a)

instance showOpaque :: Show (Opaque a) where
  show (Opaque _) =
    "Opaque"
