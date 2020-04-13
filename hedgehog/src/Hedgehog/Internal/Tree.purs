module Hedgehog.Internal.Tree (
    Tree
  , TreeT(..)
  , runTree
  , mapTreeT
  , treeValue
  , treeChildren

  , Node
  , NodeT(..)
  , fromNodeT

  , unfold
  , unfoldForest

  , expand
  , prune

  , catMaybes
  , filter
  , mapMaybe
  , filterMaybeT
  , mapMaybeMaybeT
  , filterT
  , mapMaybeT
  , depth
  , interleave

  , render
  , renderT
  ) where

import Prelude
import Prim.TypeError (class Warn, Text)

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Control.Apply (lift2)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero, guard)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Morph (class MFunctor, class MMonad, hoist, generalize)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter, tell, listen)

import Data.Identity (Identity(..))
import Data.List.Lazy (Step(..), List, (:), length, takeWhile, zipWith, concatMap, nil, iterate, repeat)
import Data.List.Lazy as List
import Data.List.Lazy.Extra (splitAt, unlines, lines)
import Data.List.Lazy.Extra as ListExtra
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl, foldr, foldMap, foldlDefault, foldrDefault)
import Data.String as String
import Data.Traversable (class Traversable, traverse, sequenceDefault)

import Hedgehog.Internal.Distributive (class MonadTransDistributive, distributeT)

------------------------------------------------------------------------

-- | A rose tree.
type Tree =
  TreeT Identity

-- | An effectful tree, each node in the tree can have an effect before it is
-- | produced.
newtype TreeT m a =
  TreeT (m (NodeT m a))
derive newtype instance eqTreeT :: (Eq (m (NodeT m a))) => Eq (TreeT m a)
derive instance newtypeTreeT :: Newtype (TreeT m a) _

-- | A node in a rose tree.
type Node =
  NodeT Identity

-- | A node in an effectful tree, as well as its unevaluated children.
newtype NodeT m a =
  NodeT {
      -- | The value at this 'NodeT' in the 'TreeT'.
      value :: a

      -- | The children of this 'NodeT'.
    , children :: List (TreeT m a)
    }
derive newtype instance eqNodeT :: (Eq (m (NodeT m a)), Eq a) => Eq (NodeT m a)
derive instance newtypeNodeT :: Newtype (NodeT m a) _

runTreeT :: forall m a. TreeT m a -> m (NodeT m a)
runTreeT (TreeT ma) =
  ma

-- | Extracts the 'Node' from a 'Tree'.
runTree :: forall a. Tree a -> Node a
runTree =
  un Identity <<< runTreeT

-- | Map between 'TreeT' computations.
mapTreeT :: forall m a. (m (NodeT m a) -> m (NodeT m a)) -> TreeT m a -> TreeT m a
mapTreeT f =
  TreeT <<< f <<< runTreeT

-- | Create a 'TreeT' from a 'NodeT'
fromNodeT :: forall m a. Applicative m => NodeT m a -> TreeT m a
fromNodeT =
  TreeT <<< pure

-- | The value at the root of the 'Tree'.
treeValue :: forall a. Tree a -> a
treeValue =
  _.value <<< un NodeT <<< runTree

-- | The children of the 'Tree'.
treeChildren :: forall a. Tree a -> List (Tree a)
treeChildren =
  _.children <<< un NodeT <<< runTree

-- | Create a tree from a value and an unfolding function.
unfold :: forall m a. Monad m => (a -> List a) -> a -> TreeT m a
unfold f x =
  TreeT <<< pure $
    NodeT {value: x, children: unfoldForest f x }

-- | Create a forest from a value and an unfolding function.
unfoldForest :: forall m a. Monad m => (a -> List a) -> a -> List (TreeT m a)
unfoldForest f =
  map (unfold f) <<< f

-- | Expand a tree using an unfolding function.
expand :: forall m a. Monad m => (a -> List a) -> TreeT m a -> TreeT m a
expand f m =
  TreeT $ do
    NodeT {value: x, children: xs } <- runTreeT m
    pure $ NodeT { value: x, children: map (expand f) xs <|> unfoldForest f x }

-- | Throw away `n` levels of a tree's children.
-- |
-- | `prune 0` will throw away all of a tree's children.
prune :: forall m a. Monad m => Int -> TreeT m a -> TreeT m a
prune n m =
  if n <= 0 then
    TreeT do
      NodeT { value: x } <- runTreeT m
      pure $ NodeT {value: x, children: nil }
  else
    TreeT do
      NodeT {value: x, children: xs0 } <- runTreeT m
      pure $ NodeT {value: x, children: map (prune (n - 1)) xs0 }

-- | Returns the depth of the deepest leaf node in the tree.
depth :: forall a. Tree a -> Int
depth m =
  let
    NodeT { children: xs } =
      runTree m

    maxDepth acc next =
      let
        d = depth next
      in
        if acc < d then
          d
        else
          acc

    n =
      foldl maxDepth 0 xs
  in
    1 + n

-- | Takes a tree of 'Maybe's and returns a tree of all the 'Just' values.
-- |
-- | If the root of the tree is 'Nothing' then 'Nothing' is returned.
catMaybes :: forall a. Tree (Maybe a) -> Maybe (Tree a)
catMaybes m =
  let
    NodeT {value: mx, children: mxs} =
      runTree m
  in
    case mx of
      Nothing -> do
        case List.step $ List.mapMaybe catMaybes mxs of
          Nil ->
            Nothing
          Cons (TreeT (Identity (NodeT {value: x, children: xs0}))) xs1 ->
            Just $ TreeT $ Identity $ NodeT
              {value: x, children: xs0 <|> xs1 }
      Just x ->
        Just $ TreeT $ Identity $ NodeT
          {value: x, children: List.mapMaybe catMaybes mxs }

fromPred :: forall a. (a -> Boolean) -> a -> Maybe a
fromPred p a = a <$ guard (p a)

-- | Returns a tree containing only elements that match the predicate.
-- |
-- | If the root of the tree does not match the predicate then 'Nothing' is
-- | returned.
filter :: forall a. (a -> Boolean) -> Tree a -> Maybe (Tree a)
filter p = mapMaybe (fromPred p)

mapMaybe :: forall a b. (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybe p =
  catMaybes <<<
  runTreeMaybeT <<<
  mapMaybeMaybeT p <<<
  hoist lift

runTreeMaybeT :: forall m a. Monad m => TreeT (MaybeT m) a -> TreeT m (Maybe a)
runTreeMaybeT =
  runMaybeT <<<
  distributeT

-- | Returns a tree containing only elements that match the predicate.
-- |
-- | If the root of the tree does not match the predicate then 'Nothing' is
-- | returned.
filterMaybeT :: forall a. (a -> Boolean) -> TreeT (MaybeT Identity) a -> TreeT (MaybeT Identity) a
filterMaybeT p = mapMaybeMaybeT (fromPred p)

mapMaybeMaybeT :: forall a b. (a -> Maybe b) -> TreeT (MaybeT Identity) a -> TreeT (MaybeT Identity) b
mapMaybeMaybeT p t =
  case runTreeMaybeT t of
    TreeT (Identity (NodeT {value: Nothing, children: _ })) ->
      TreeT $ MaybeT $ Identity $ Nothing
    TreeT (Identity (NodeT {value: Just x, children: xs })) ->
      case p x of
        Nothing ->
          TreeT $ MaybeT $ Identity $ Nothing
        Just x' ->
          hoist generalize $ TreeT $ Identity $ NodeT
              { value: x', children: concatMap (flattenTree p) xs }

flattenTree :: forall a b. (a -> Maybe b) -> Tree (Maybe a) -> List (Tree b)
flattenTree p (TreeT (Identity (NodeT {value: mx, children: mxs0}))) =
  let
    mxs =
      concatMap (flattenTree p) mxs0
  in
    case mx of
      Nothing ->
        mxs
      Just x ->
        case p x of
          Just x' ->
            (TreeT (Identity (NodeT {value: x', children: mxs}))) : nil
          Nothing ->
            mxs

-- | Returns a tree containing only elements that match the predicate.
-- |
-- | When an element does not match the predicate its node is replaced with
-- | 'empty'.
filterT :: forall m a
   . Monad m
  => Alternative m
  => (a -> Boolean)
  -> TreeT m a
  -> TreeT m a
filterT p =
  mapMaybeT (fromPred p)

mapMaybeT :: forall m a b
   . Monad m
  => Alternative m
  => (a -> Maybe b)
  -> TreeT m a
  -> TreeT m b
mapMaybeT p m =
  TreeT do
    NodeT { value: x, children: xs } <- runTreeT m
    case p x of
      Just x' ->
        pure $
          NodeT { value: x', children: (map (mapMaybeT p) xs) }
      Nothing ->
        empty

------------------------------------------------------------------------

-- | All ways a list can be split
-- |
-- | > splits [1,2,3]
-- | > ==
-- | > [ ([], 1, [2, 3])
-- |   , ([1], 2, [3])
-- |   , ([1, 2], 3, [])
-- |   ]
splits :: forall a. List a -> List { before :: List a, focus :: a, after :: List a }
splits xs0 =
  let
    go l1 l2 = case List.step l1, List.step l2 of
      Cons front fronts, Cons x xs ->
        {before: front, focus: x, after: xs} : go fronts xs
      _, _ ->
        nil
  in
    go (ListExtra.inits xs0) xs0

-- | `removes n` computes all ways we can remove chunks of size `n` from a list
-- |
-- Examples
-- |
-- | > removes 1 [1..3] == [[2,3],[1,3],[1,2]]
-- | > removes 2 [1..4] == [[3,4],[1,2]]
-- | > removes 2 [1..5] == [[3,4,5],[1,2,5],[1,2,3,4]]
-- | > removes 3 [1..5] == [[4,5],[1,2,3]]
-- |
-- Note that the last chunk we delete might have fewer elements than `n`.
removes :: forall a. Int -> List a -> List (List a)
removes k = \xs -> go xs
  where
    go :: List a -> List (List a)
    go l =
      case List.step l of
        Nil -> nil
        _ ->
          let
            Tuple xs1 xs2 = splitAt k l
          in
            xs2 : map (\list -> xs1 <|> list) (go xs2)

dropSome :: forall m a. Monad m => List (NodeT m a) -> List (TreeT m (List a))
dropSome ts = do
  n   <- takeWhile (_ > 0) $ iterate (_ `div` 2) (length ts)
  ts' <- removes n ts
  pure $ TreeT $ pure $ interleave ts'

shrinkOne :: forall m a. Monad m => List (NodeT m a) -> List (TreeT m (List a))
shrinkOne ts = do
  {before: xs, focus: (NodeT y0), after: zs} <- splits ts
  y1 <- y0.children
  pure $ TreeT do
    y2 <- runTreeT y1
    pure $
      interleave (xs <|> (y2 : zs))

interleave :: forall m a. Monad m => List (NodeT m a) -> NodeT m (List a)
interleave ts =
  NodeT {
      value: map (_.value <<< un NodeT) ts
    , children: ListExtra.concat $ dropSome ts : shrinkOne ts : nil
  }

------------------------------------------------------------------------

instance foldableTree :: Foldable (TreeT Identity) where
  foldl f init (TreeT mx) =
    foldl f init (un Identity mx)

  foldr f last (TreeT mx) =
    foldr f last (un Identity mx)

  foldMap f (TreeT mx) =
    foldMap f (un Identity mx)

instance foldableNodeTIdentity :: Foldable (NodeT Identity) where
  foldl f init nodeT =
    foldlDefault f init nodeT

  foldr f last nodeT =
    foldrDefault f last nodeT

  foldMap f (NodeT {value: x, children: xs}) =
    append (f x) (foldr append mempty (map (foldMap f) xs))

instance traversableTreeTIdentity :: Traversable (TreeT Identity) where
  traverse f (TreeT mx) =
    TreeT <$> traverse (traverse f) mx

  sequence =
    sequenceDefault

instance traversableNodeT :: Traversable (NodeT Identity) where
  traverse f (NodeT {value: x, children: xs}) =
    (\value children -> NodeT { value, children })
      <$> f x <*> traverse (traverse f) xs

  sequence =
    sequenceDefault

------------------------------------------------------------------------
-- NodeT/TreeT instances

derive instance functorNodeT :: Functor m => Functor (NodeT m)

derive instance functorTreeT :: Functor m => Functor (TreeT m)

instance applyNodeT :: Applicative m => Apply (NodeT m) where
  apply (NodeT {value: ab, children: tabs}) na@(NodeT {value: a, children: tas}) =
    NodeT { value: ab a
          , children: map (_ <*> (fromNodeT na)) tabs <|> map (map ab) tas
          }

instance applyTreeT :: Applicative m => Apply (TreeT m) where
  apply (TreeT mab) (TreeT ma) =
    TreeT $
      lift2 (<*>) mab ma

instance applicativeNodeT :: Applicative m => Applicative (NodeT m) where
  pure x = NodeT {value: x, children: nil}

instance applicativeTreeT :: Applicative m => Applicative (TreeT m) where
  pure = TreeT <<< pure <<< pure

instance bindNodeT :: Monad m => Bind (NodeT m) where
  bind (NodeT { value: x, children: xs }) k =
    case k x of
      NodeT { value: y, children: ys } ->
        NodeT {
            value: y
          , children: map (TreeT <<< map (_ >>= k) <<< runTreeT) xs <|> ys
          }

instance bindTreeT :: Monad m => Bind (TreeT m) where
  bind m k =
    TreeT $ do
      NodeT { value: x, children: xs } <- runTreeT m
      NodeT { value: y, children: ys } <- runTreeT (k x)
      pure $ NodeT { value: y, children: map (_ >>= k) xs <|> ys }

instance monadNodeT :: Monad m => Monad (NodeT m)

instance monadTreeT :: Monad m => Monad (TreeT m)

instance altTreeT :: Alt m => Alt (TreeT m) where
  alt x y = TreeT (runTreeT x <|> runTreeT y)

instance plusTreeT :: Plus m => Plus (TreeT m) where
  empty = TreeT empty

instance alternativeTreeT :: Alternative m => Alternative (TreeT m)

instance monadZeroTreeT :: MonadZero m => MonadZero (TreeT m)

instance monadPlusTreeT :: MonadPlus m => MonadPlus (TreeT m)

zipTreeT :: forall f a b. Applicative f => TreeT f a -> TreeT f b -> TreeT f (Tuple a b)
zipTreeT l0@(TreeT left) r0@(TreeT right) =
  TreeT $
    let
      zipNodeT :: NodeT f a -> NodeT f b -> NodeT f (Tuple a b)
      zipNodeT (NodeT {value:a, children: ls}) (NodeT {value: b, children: rs}) =
        NodeT { value: Tuple a b
              , children: ListExtra.concat $
                    (ls >>= \l1 -> pure $ zipTreeT l1 r0)
                  : (rs >>= \r1 -> pure $ zipTreeT l0 r1)
                  : nil
              }
    in
      zipNodeT <$> left <*> right

instance monadTransTreeT :: MonadTrans TreeT where
  lift f =
    TreeT $
      map (\x -> NodeT {value: x, children: nil}) f

instance mFunctorNodeT :: MFunctor NodeT where
  hoist f (NodeT {value: x, children: xs}) =
    NodeT { value: x, children: map (hoist f) xs }

instance mFunctorTreeT :: MFunctor TreeT where
  hoist f (TreeT m) =
    TreeT <<< f $ map (hoist f) m

embedNodeT :: forall m t b. Monad m => (t (NodeT t b) -> TreeT m (NodeT t b)) -> NodeT t b -> NodeT m b
embedNodeT f (NodeT {value: x, children: xs}) =
  NodeT { value: x, children: map (embedTreeT f) xs }

embedTreeT :: forall m t b. Monad m => (t (NodeT t b) -> TreeT m (NodeT t b)) -> TreeT t b -> TreeT m b
embedTreeT f (TreeT m) =
  TreeT <<< pure <<< embedNodeT f =<< f m

instance mMonadTreeT :: MMonad TreeT where
  embed f m =
    embedTreeT f m

distributeNodeT :: forall t m a
   . Monad m
  => Monad (t m)
  => Monad (TreeT m)
  => Monad (t (TreeT m))
  => MonadTrans t
  => MFunctor t
  => NodeT (t m) a
  -> t (TreeT m) a
distributeNodeT (NodeT {value: x, children: xs}) =
  join $ lift $ fromNodeT $
    NodeT {
          value: pure x
        , children: map (pure <<< distributeTreeT) xs
        }

distributeTreeT :: forall t m a
   . Monad m
  => Monad (t m)
  => Monad (TreeT m)
  => Monad (t (TreeT m))
  => MonadTrans t
  => MFunctor t
  => TreeT (t m) a
  -> t (TreeT m) a
distributeTreeT x =
  distributeNodeT =<< hoist lift (runTreeT x)

instance monadTransDistributiveTreeT :: MonadTransDistributive TreeT where
  distributeT =
    distributeTreeT

instance monadEffectTreeT :: MonadEffect m => MonadEffect (TreeT m) where
  liftEffect =
    lift <<< liftEffect

instance monadAffTreeT :: MonadAff m => MonadAff (TreeT m) where
  liftAff =
    lift <<< liftAff

instance monadThrowTreeT :: MonadThrow e m => MonadThrow e (TreeT m) where
  throwError =
    lift <<< throwError

handleNodeT :: forall e m a. MonadError e m => (e -> TreeT m a) -> NodeT m a -> NodeT m a
handleNodeT onErr (NodeT {value: x, children: xs}) =
  NodeT { value: x, children: map (handleTreeT onErr) xs }

handleTreeT :: forall e m a. MonadError e m => (e -> TreeT m a) -> TreeT m a -> TreeT m a
handleTreeT onErr m =
  TreeT <<< map (handleNodeT onErr) $
    catchError (runTreeT m) (runTreeT <<< onErr)

instance monadErrorTreeT :: MonadError e m => MonadError e (TreeT m) where
  catchError =
    flip handleTreeT

localNodeT :: forall r m a. MonadReader r m => (r -> r) -> NodeT m a -> NodeT m a
localNodeT f (NodeT {value: x, children: xs}) =
  NodeT {value: x, children: map (localTreeT f) xs }

localTreeT :: forall r m a. MonadReader r m => (r -> r) -> TreeT m a -> TreeT m a
localTreeT f (TreeT m) =
  TreeT $
    pure <<< localNodeT f =<< local f m

instance monadAskTreeT :: MonadAsk r m => MonadAsk r (TreeT m) where
  ask =
    lift ask

instance monadReaderTreeT :: MonadReader r m => MonadReader r (TreeT m) where
  local =
    localTreeT

instance monadStateTreeT :: MonadState s m => MonadState s (TreeT m) where
  state =
    lift <<< state

listenNodeT :: forall w m a. Semigroup w => MonadWriter w m => w -> NodeT m a -> NodeT m (Tuple a w)
listenNodeT w (NodeT {value: x, children: xs}) =
  NodeT { value: (Tuple x w), children: map (listenTreeT w) xs }

listenTreeT :: forall w m a. Semigroup w => MonadWriter w m => w -> TreeT m a -> TreeT m (Tuple a w)
listenTreeT w0 (TreeT m) =
  TreeT do
    Tuple x w <- listen m
    pure $ listenNodeT (append w0 w) x

-- FIXME This just throws away the writer modification function.
passNodeT :: forall w m a. MonadWriter w m => NodeT m (Tuple a (w -> w)) -> NodeT m a
passNodeT (NodeT {value: (Tuple x _), children: xs}) =
  NodeT { value: x, children: map passTreeT xs }

passTreeT :: forall w m a. MonadWriter w m => TreeT m (Tuple a (w -> w)) -> TreeT m a
passTreeT (TreeT m) =
  TreeT $
    pure <<< passNodeT =<< m

instance monadTellTreeT :: MonadTell w m => MonadTell w (TreeT m) where
  tell =
    lift <<< tell

instance monadWriterTreeT :: (Monoid w, MonadWriter w m) => MonadWriter w (TreeT m) where
  listen =
    listenTreeT mempty
  pass =
    passTreeT

-- Duplicate of handleNodeT
handleErrorNodeT :: forall e m a. MonadError e m => (e -> TreeT m a) -> NodeT m a -> NodeT m a
handleErrorNodeT onErr (NodeT {value: x, children: xs}) =
  NodeT {value: x, children: map (handleErrorTreeT onErr) xs }

-- Duplicate of handleTreeT
handleErrorTreeT :: forall e m a. MonadError e m => (e -> TreeT m a) -> TreeT m a -> TreeT m a
handleErrorTreeT onErr m =
  TreeT <<< map (handleErrorNodeT onErr) $
    catchError (runTreeT m) (runTreeT <<< onErr)

------------------------------------------------------------------------
-- Show/Show1 instances

instance showNodeT :: (Warn (Text "Show instance for NodeT not yet implemented"), Show (m a), Show a) => Show (NodeT m a) where
  show _ = "Show instance for NodeT has not yet been implemented"

instance showTreeT :: (Warn (Text "Show instance for TreeT not yet implemented"), Show (m a), Show a) => Show (TreeT m a) where
  show _ = "Show instance for TreeT has not yet been implemented"

------------------------------------------------------------------------
-- Pretty Printing

-- |
-- Rendering implementation based on the one from containers/Data.Tree

renderTreeTLines :: forall m. Monad m => TreeT m String -> m (List String)
renderTreeTLines (TreeT m) = do
  NodeT {value: x, children: xs0} <- m
  xs <- renderForestLines xs0
  pure $
    lines (renderNodeT x) <|> xs

renderNodeT :: String -> String
renderNodeT str =
  if String.length str == 1 then
    " " <> str
  else
    str

renderForestLines :: forall m. Monad m => List (TreeT m String) -> m (List String)
renderForestLines xs0 =
  -- let
  --   Port Note: this produces a compiler error. I'm not sure how to fix it.
  --   shift hd other =
  --     zipWith (<|>) (hd : repeat other)
  -- in
    case List.step xs0 of
      Nil ->
        pure nil

      Cons x rest -> do
        s <- renderTreeTLines x
        case List.step rest of
          Nil -> do
            -- Port Note: pure $ shift " └╼" "   " s
            pure $ zipWith (<>) (" └╼" : repeat "   ") s
          _ -> do
            ss <- renderForestLines rest
            -- Port Note: pure $ shift " ├╼" " │ " (s <|> ss)
            pure $ zipWith (<>) (" ├╼" : repeat " │ ") (s <|> ss)

-- | Render a tree of strings.
render :: Tree String -> String
render =
  un Identity <<< renderT

-- | Render a tree of strings, note that this forces all the delayed effects in
-- | the tree.
renderT :: forall m. Monad m => TreeT m String -> m String
renderT =
  map unlines <<< renderTreeTLines
