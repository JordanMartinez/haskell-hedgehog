module Hedgehog.Internal.Shrink (
    towards
  , towardsFloat
  , list

  , halves
  , removes
  , consNub
  ) where


-- | Shrink an integral number by edging towards a destination.
-- |
-- | >>> towards 0 100
-- | [0,50,75,88,94,97,99]
-- |
-- | >>> towards 500 1000
-- | [500,750,875,938,969,985,993,997,999]
-- |
-- | >>> towards (-50) (-26)
-- | [-50,-38,-32,-29,-27]
-- |
-- | /Note we always try the destination first, as that is the optimal shrink./
towards :: forall a. Integral a => a -> a -> List a
towards destination x =
  if destination == x then
    Nil
  else
    let
      -- Halve the operands before subtracting them so they don't overflow.
      -- Consider 'minBound' and 'maxBound' for a fixed sized type like 'Int64'.
      diff = (x `quot` 2) - (destination `quot` 2)
    in
      destination `consNub` fmap (x - _) (halves diff)

-- | Shrink a floating-point number by edging towards a destination.
-- |
-- | >>> take 7 (towardsFloat 0.0 100)
-- | [0.0,50.0,75.0,87.5,93.75,96.875,98.4375]
-- |
-- | >>> take 7 (towardsFloat 1.0 0.5)
-- | [1.0,0.75,0.625,0.5625,0.53125,0.515625,0.5078125]
-- |
-- | /Note we always try the destination first, as that is the optimal shrink./
towardsFloat :: RealFloat a => a -> a -> List a
towardsFloat destination x =
  if destination == x then
    Nil
  else
    let
      diff = x - destination

      ok y = y /= x && not (isNaN y) && not (isInfinite y)
    in
      takeWhile ok <<<
      fmap (x - _) $
      iterate (_ / 2) diff

-- | Shrink a list by edging towards the empty list.
-- |
-- | >>> list [1,2,3]
-- | [[],[2,3],[1,3],[1,2]]
-- |
-- | >>> list "abcd"
-- | ["","cd","ab","bcd","acd","abd","abc"]
-- |
-- | /Note we always try the empty list first, as that is the optimal shrink./
list :: List a -> [List a]
list xs =
 concatMap
   (\k -> removes k xs)
   (halves $ length xs)

-- | Produce all permutations of removing 'k' elements from a list.
-- |
-- | >>> removes 2 "abcdef"
-- | ["cdef","abef","abcd"]
removes :: Int -> List a -> List (List a)
removes k0 xs0 =
  let
    loop k n xs =
      let
        Tuple hd tl = splitAt k xs -- API not yet exist for Lists/Array
      in
        if k > n then
          Nil
        else if null tl then
          Cons Nil Nil
        else
          tl : fmap (\list -> hd <|> list) (loop k (n - k) tl)
  in
    loop k0 (length xs0) xs0

-- | Produce a list containing the progressive halving of an integral.
-- |
-- | >>> halves 15
-- | [15,7,3,1]
-- |
-- | >>> halves 100
-- | [100,50,25,12,6,3,1]
-- |
-- | >>> halves (-26)
-- | [-26,-13,-6,-3,-1]
halves :: forall a. Integral a => a -> List a
halves =
  takeWhile (_ /= 0) <<< iterate (`quot` 2)

-- | Cons an element on to the front of a list unless it is already there.
consNub :: forall a. Eq a => a -> List a -> List a
consNub x ys0 =
  case ys0 of
    Nil ->
      x : Nil
    Cons y ys ->
      if x == y then
        y : ys
      else
        x : y : ys
