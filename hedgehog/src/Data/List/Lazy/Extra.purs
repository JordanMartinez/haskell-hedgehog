-- | This module represents functions that are implemented in Haskell,
-- | but which aren't implemented in PureScript's corresponding module
-- | for lazy linked-lists.
-- |
-- | Some of these functions have not yet been implemented as indicated
-- | by the `Warn` type class.
module Data.List.Lazy.Extra where

import Prelude

import Prim.TypeError (class Warn, Text)
import Control.Alt ((<|>))
import Data.Tuple (Tuple(..))
import Data.List.Lazy (List, fromFoldable, (:), nil)
import Data.Foldable (class Foldable, intercalate, foldl)
import Data.String.Utils as StringUtils

inits :: forall a
       . Warn (Text "`Data.List.Extra.inits` is not yet implemented")
      => List a
      -> List (List a)
inits _ = nil

splitAt :: forall a
       . Warn (Text "`Data.List.Extra.splitAt` is not yet implemented")
      => Int
      -> List a
      -> Tuple (List a) (List a)
splitAt _ _ = Tuple nil nil

unlines :: forall f. Foldable f => f String -> String
unlines = intercalate "\n"

lines :: String -> List String
lines = fromFoldable <<< StringUtils.lines

concat :: forall f a. Foldable f => f (List a) -> List a
concat container =
  let
    reversedContainer = foldl (\acc next -> next : acc) nil container

    combineLists = foldl (\acc next -> next <|> acc) nil reversedContainer

    originalOrder = foldl (\acc next -> next : acc) nil combineLists

  in
    originalOrder
