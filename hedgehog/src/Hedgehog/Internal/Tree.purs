module Hedgehog.Internal.Tree (
    Tree
  -- , pattern Tree
  , TreeT(..)
  , runTree
  , mapTreeT
  , treeValue
  , treeChildren

  , Node
  -- , pattern Node
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
import Control.MonadPlus (class MonadPlus)
-- import Control.Monad.Base (MonadBase(..))
-- import Control.Monad.Trans.Control ()
import Control.Monad.Error.Class (class MonadThrow, class MonadError)
import Effect.Class (class MonadEffect)
import Control.Monad.Morph (MFunctor(..), MMonad(..), generalize)
-- import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Maybe.Trans (MaybeT(..))
-- import Control.Monad.Resource.Trans (class MonadResource)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
-- import Control.Monad.Zip (MonadZip(..))

import Data.Identity (Identity(..))
import Data.Eq (class Eq1)
-- import Data.Functor.Classes (Show1(..), showsPrec1)
-- import Data.Functor.Classes (showsUnaryWith, showsBinaryWith)
import Data.List as List
import Data.Maybe as Maybe

import Hedgehog.Internal.Distributive
-- import Control.Monad.Trans.Control (MonadBaseControl (..))

------------------------------------------------------------------------

-- | A rose tree.
type Tree = TreeT Identity

-- Pattern to ease construction / deconstruction of pure trees.
--
-- pattern Tree :: NodeT Identity a -> Tree a
-- pattern Tree node =
--   TreeT (Identity node)
-- #if __GLASGOW_HASKELL__ >= 802
-- #endif

-- | An effectful tree, each node in the tree can have an effect before it is
-- | produced.
newtype TreeT m a = TreeT (m (NodeT m a))
derive newtype instance eqTreeT :: (Eq (m a)) => Eq (TreeT m a)

-- instance MonadBaseControl b m => MonadBaseControl b (TreeT m) where
--   type StM (TreeT m) a = StM m (NodeT m a)
--   liftBaseWith f = TreeT $ liftBaseWith (\g -> pure <$> f (g <<< runTreeT))
--   restoreM = TreeT <<< restoreM

-- | A node in a rose tree.
type Node = NodeT Identity
-- #if __GLASGOW_HASKELL__ >= 802
-- #endif

-- Pattern to ease construction / deconstruction of pure nodes.
--
-- pattern Node :: a -> [Tree a] -> Node a
-- pattern Node x xs =
--   NodeT x xs

-- | A node in an effectful tree, as well as its unevaluated children.
newtype NodeT m a =
  NodeT {
      -- | The value at this 'NodeT' in the 'TreeT'.
      value :: a

      -- | The children of this 'NodeT'.
    , children :: List (TreeT m a)
    }
derive newtype instance eqNodeT :: (Eq a) => Eq (NodeT m a)

-- | Extracts the 'Node' from a 'Tree'.
runTree :: forall a. Tree a -> Node a
runTree = runIdentity <<< runTreeT

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
  _.value <<< runTree

-- | The children of the 'Tree'.
treeChildren :: forall a. Tree a -> List (Tree a)
treeChildren =
  _.children <<< runTree

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
-- | /`prune 0` will throw away all of a tree's children./
prune :: forall m a. Monad m => Int -> TreeT m a -> TreeT m a
prune n m =
  if n <= 0 then
    TreeT $ do
      NodeT { value: x } <- runTreeT m
      pure $ NodeT {value: x, children: Nil }
  else
    TreeT $ do
      NodeT {value: x, children: xs0 } <- runTreeT m
      pure $ NodeT {value: x, children: map (prune (n - 1)) xs0 }

-- | Returns the depth of the deepest leaf node in the tree.
depth :: forall a. Tree a -> Int
depth m =
  let
    NodeT { children: xs } =
      runTree m

    n =
      if null xs then
        0
      else
        maximum (map depth xs)
  in
    1 + n

-- | Takes a tree of 'Maybe's and returns a tree of all the 'Just' values.
-- |
-- | If the root of the tree is 'Nothing' then 'Nothing' is returned.
catMaybes :: forall a. Tree (Maybe a) -> Maybe (Tree a)
catMaybes m =
  let
    NodeT {value: mx, children: mxs} = runTree m
  in
    case mx of
      Nothing -> do
        case Maybe.mapMaybe catMaybes mxs of
          Nil ->
            Nothing
          Cons (TreeT (Identity (NodeT x xs0))) xs1 ->
            Just <<< Tree $
              NodeT {value: x, children: xs0 ++ xs1 }
      Just x ->
        Just <<< Tree $
          NodeT {value: x, children: Maybe.mapMaybe catMaybes mxs }

fromPred :: forall a. (a -> Bool) -> a -> Maybe a
fromPred p a = a <$ guard (p a)

-- | Returns a tree containing only elements that match the predicate.
-- |
-- | If the root of the tree does not match the predicate then 'Nothing' is
-- | returned.
filter :: forall a. (a -> Bool) -> Tree a -> Maybe (Tree a)
filter p = mapMaybe (fromPred p)

mapMaybe :: forall a. (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybe p =
  catMaybes <<<
  runTreeMaybeT <<<
  mapMaybeMaybeT p <<<
  hoist lift

runTreeMaybeT :: forall m a. Monad m => TreeT (MaybeT m) a -> TreeT m (Maybe a)
runTreeMaybeT = runMaybeT <<< distributeT

-- | Returns a tree containing only elements that match the predicate.
-- |
-- | If the root of the tree does not match the predicate then 'Nothing' is
-- | returned.
filterMaybeT :: forall a. (a -> Bool) -> TreeT (MaybeT Identity) a -> TreeT (MaybeT Identity) a
filterMaybeT p = mapMaybeMaybeT (fromPred p)

mapMaybeMaybeT :: forall a b. (a -> Maybe b) -> TreeT (MaybeT Identity) a -> TreeT (MaybeT Identity) b
mapMaybeMaybeT p t =
  case runTreeMaybeT t of
    TreeT (Identity (Node {value: Nothing })) ->
      TreeT <<< MaybeT <<< Identity $ Nothing
    TreeT (Identity (Node {value: Just x, children: xs })) ->
      case p x of
        Nothing -> TreeT <<< MaybeT <<< Identity $ Nothing
        Just x' ->
          hoist generalize $
            Tree <<< Node x' $
              concatMap (flattenTree p) xs

flattenTree :: forall a b. (a -> Maybe b) -> Tree (Maybe a) -> List (Tree b)
flattenTree p (TreeT (Identity (NodeT {value: mx, children: mxs0}))) =
  let
    mxs =
      concatMap (flattenTree p) mxs0
  in
    case mx of
      Nothing -> mxs
      Just x ->
        case p x of
          Just x' ->
            Cons (TreeT (Identity (NodeT x' mxs))) Nil
          Nothing ->
            mxs

-- | Returns a tree containing only elements that match the predicate.
-- |
-- | When an element does not match the predicate its node is replaced with
-- | 'empty'.
filterT :: forall m a. Monad m => Alternative m => (a -> Bool) -> TreeT m a -> TreeT m a
filterT p =
  mapMaybeT (fromPred p)

mapMaybeT :: forall m a. Monad m => Alternative m => (a -> Maybe b) -> TreeT m a -> TreeT m b
mapMaybeT p m =
  TreeT $ do
    NodeT x xs <- runTreeT m
    case p x of
      Just x' ->
        pure $
          NodeT x' (map (mapMaybeT p) xs)
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
    go (Cons front fronts) (Cons x xs) =
      Cons {before: front, focus: x, after: xs} : go fronts xs
    go _ _ =
      Nil
  in
    go (List.inits xs0) xs0

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
removes :: forall a. Int -> List a -> List (List a))
removes k = \xs -> go xs
  where
    go :: List a -> List (List a))
    go Nil = Nil
    go xs = xs2 : map (\list -> xs1 <|> list) (go xs2)
      where
        Tuple xs1 xs2 = splitAt k xs

dropSome :: forall m a. Monad m => List (NodeT m a) -> List (TreeT m (List a))
dropSome ts = do
  n   <- takeWhile (_ > 0) $ iterate (_ `div` 2) (length ts)
  ts' <- removes n ts
  pure <<< TreeT <<< pure $ interleave ts'

shrinkOne :: forall m a. Monad m => List (NodeT m a) -> List (TreeT m (List a))
shrinkOne ts = do
  {before: xs, focus: y0, after: zs} <- splits ts
  y1 <- nodeChildren y0
  pure <<< TreeT $ do
    y2 <- runTreeT y1
    pure $
      interleave (xs <|> (y2 `cons` zs))

interleave :: forall m a. Monad m => List (NodeT m a) -> NodeT m (List a)
interleave ts =
  NodeT (map nodeValue ts) $
    concat [
        dropSome ts
      , shrinkOne ts
      ]

------------------------------------------------------------------------

instance foldableTree :: Foldable (TreeT Identity) where
  foldMap f (TreeT mx) =
    foldMap f (runIdentity mx)

instance foldableNode :: Foldable (NodeT Identity) where
  foldMap f (NodeT x xs) =
    f x `mappend` mconcat (map (foldMap f) xs)

instance traversableTreeT :: Traversable (TreeT Identity) where
  traverse f (TreeT mx) =
    TreeT <$> traverse (traverse f) mx

instance traversableNodeT :: Traversable (NodeT Identity) where
  traverse f (NodeT x xs) =
    NodeT <$> f x <*> traverse (traverse f) xs

------------------------------------------------------------------------
-- NodeT/TreeT instances

instance eqTreeT :: (Eq1 m, Eq a) => Eq (TreeT m a) where
  TreeT m0 == TreeT m1 =
    liftEq (==) m0 m1

instance functorNodeT :: Functor m => Functor (NodeT m) where
  map f (NodeT x xs) =
    NodeT (f x) (map (map f) xs)

instance functorTreeT :: Functor m => Functor (TreeT m) where
  map f =
    TreeT <<< map (map f) <<< runTreeT

instance applyNodeT :: Apply m => Apply (NodeT m) where
  apply (NodeT ab tabs) na`(NodeT a tas) =
    NodeT (ab a) $
      map (<*> (fromNodeT na)) tabs ++ map (map ab) tas

instance applyTreeT :: Apply m => Apply (TreeT m) where
  apply (TreeT mab) (TreeT ma) =
    TreeT $
      liftA2 (<*>) mab ma

instance applicativeNodeT :: Applicative m => Applicative (NodeT m) where
  pure x = NodeT x Nil

instance applicativeTreeT :: Applicative m => Applicative (TreeT m) where
  pure = TreeT <<< pure <<< pure

instance monadNodeT :: Monad m => Monad (NodeT m) where
  bind (NodeT x xs) k =
    case k x of
      NodeT y ys ->
        NodeT y $
map (TreeT <<< map (>>= k) <<< runTreeT) xs <|> ys

instance bindTreeT :: Bind m => Bind (TreeT m) where
  bind m k =
    TreeT $ do
      NodeT x xs <- runTreeT m
      NodeT y ys <- runTreeT (k x)
      pure <<< NodeT y $
        map (_ >>= k) xs <|> ys

instance monadNodeT :: Monad m => Monad (NodeT m)

instance monadTreeT :: Monad m => Monad (TreeT m)

instance altTreeT :: Alt m => Alt (TreeT m) where
  alt x y = TreeT (runTreeT x <|> runTreeT y)

instance plusTreeT :: Plus m => Plus (TreeT m) where
  empty = TreeT empty

instance alternativeTreeT :: Alternative m => Alternative (TreeT m)

instance monadZeroTreeT :: MonadZero m => MonadZero (TreeT m) where
  mzero = TreeT mzero

instance monadPlusTreeT :: MonadPlus m => MonadPlus (TreeT m) where
  mplus x y = TreeT (runTreeT x `mplus` runTreeT y)

zipTreeT :: forall f a b. Applicative f => TreeT f a -> TreeT f b -> TreeT f (Tuple a b)
zipTreeT l0`(TreeT left) r0@(TreeT right) =
  TreeT $
    let
      zipNodeT :: NodeT f a -> NodeT f b -> NodeT f (a, b)
      zipNodeT (NodeT a ls) (NodeT b rs) =
        NodeT (Tuple a b) $
          concat [ (bind ls (\l1 -> zipTreeT l1 r0))
                 , (bind rs (\r1 -> zipTreeT l0 r1))
                 ]
    in
      zipNodeT <$> left <*> right

-- PureScript doesn't have this type class
-- instance Monad m => MonadZip (TreeT m) where
--   mzip = zipTreeT

instance monadTransTreeT :: MonadTrans TreeT where
  lift f =
    TreeT $
      map (\x -> NodeT x Nil) f

instance mFunctorNodeT :: MFunctor NodeT where
  hoist f (NodeT x xs) =
    NodeT x (map (hoist f) xs)

instance mFunctorTreeT :: MFunctor TreeT where
  hoist f (TreeT m) =
    TreeT <<< f $ map (hoist f) m

embedNodeT :: forall m t b. Monad m => (t (NodeT t b) -> TreeT m (NodeT t b)) -> NodeT t b -> NodeT m b
embedNodeT f (NodeT x xs) =
  NodeT x (map (embedTreeT f) xs)

embedTreeT :: forall m t b. Monad m => (t (NodeT t b) -> TreeT m (NodeT t b)) -> TreeT t b -> TreeT m b
embedTreeT f (TreeT m) =
  TreeT <<< pure <<< embedNodeT f =<< f m

instance mMonadTreeT :: MMonad TreeT where
  embed f m = embedTreeT f m

distributeNodeT :: forall t m a. Transformer t TreeT m => NodeT (t m) a -> t (TreeT m) a
distributeNodeT (NodeT x xs) =
  join <<< lift <<< fromNodeT <<< NodeT (pure x) $
    map (pure <<< distributeTreeT) xs

distributeTreeT :: forall t m a. Transformer t TreeT m => TreeT (t m) a -> t (TreeT m) a
distributeTreeT x =
  distributeNodeT =<< hoist lift (runTreeT x)

instance monadTransDistributiveTreeT :: MonadTransDistributive TreeT where
  distributeT =
    distributeTreeT

-- PureScript doesn't have this type class and won't in the future AFAIK
-- instance PrimMonad m => PrimMonad (TreeT m) where
--   type PrimState (TreeT m) =
--     PrimState m
--   primitive =
--     lift <<< primitive

instance MonadEffect m => MonadEffect (TreeT m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (TreeT m) where
  liftAff = lift <<< liftAff

-- instance MonadBase b m => MonadBase b (TreeT m) where
--   liftBase =
--     lift <<< liftBase

instance MonadThrow m => MonadThrow (TreeT m) where
  throwM = lift <<< throwM

handleNodeT :: forall e m a. MonadCatch e m => (e -> TreeT m a) -> NodeT m a -> NodeT m a
handleNodeT onErr (NodeT x xs) =
  NodeT x $
    map (handleTreeT onErr) xs

handleTreeT :: forall e m a. MonadCatch e m => (e -> TreeT m a) -> TreeT m a -> TreeT m a
handleTreeT onErr m =
  TreeT <<< map (handleNodeT onErr) $
    catch (runTreeT m) (runTreeT <<< onErr)

instance monadCatchTreeT :: MonadCatch m => MonadCatch (TreeT m) where
  catch = flip handleTreeT

localNodeT :: forall r m a. MonadReader r m => (r -> r) -> NodeT m a -> NodeT m a
localNodeT f (NodeT x xs) =
  NodeT x $
    map (localTreeT f) xs

localTreeT :: forall r m a. MonadReader r m => (r -> r) -> TreeT m a -> TreeT m a
localTreeT f (TreeT m) =
  TreeT $
    pure <<< localNodeT f =<< local f m

instance monadAskTreeT :: MonadAsk r m => MonadAsk r (TreeT m) where
  ask = lift ask

instance monadReaderTreeT :: MonadReader r m => MonadReader r (TreeT m) where
  local = localTreeT

instance monadStateTreeT :: MonadState s m => MonadState s (TreeT m) where
  state = lift <<< state

listenNodeT :: forall w m a. MonadWriter w m => w -> NodeT m a -> NodeT m (Tuple a w)
listenNodeT w (NodeT x xs) =
  NodeT (x, w) $
    map (listenTreeT w) xs

listenTreeT :: forall w m a. MonadWriter w m => w -> TreeT m a -> TreeT m (Tuple a w)
listenTreeT w0 (TreeT m) =
  TreeT $ do
    (x, w) <- listen m
    pure $ listenNodeT (mappend w0 w) x

-- FIXME This just throws away the writer modification function.
passNodeT :: forall w m a. MonadWriter w m => NodeT m (Tuple a (w -> w)) -> NodeT m a
passNodeT (NodeT (x, _) xs) =
  NodeT x $
    map passTreeT xs

passTreeT :: forall w m a. MonadWriter w m => TreeT m (Tuple a (w -> w)) -> TreeT m a
passTreeT (TreeT m) =
  TreeT $
    pure <<< passNodeT =<< m

instance monadTellTreeT :: MonadTell w m => MonadTell w (TreeT m) where
  tell = lift <<< tell

instance monadWriterTreeT :: MonadWriter w m => MonadWriter w (TreeT m) where
  listen = listenTreeT mempty
  pass = passTreeT

handleErrorNodeT :: forall e m a. MonadError e m => (e -> TreeT m a) -> NodeT m a -> NodeT m a
handleErrorNodeT onErr (NodeT x xs) =
  NodeT x $
    map (handleErrorTreeT onErr) xs

handleErrorTreeT :: forall e m a. MonadError e m => (e -> TreeT m a) -> TreeT m a -> TreeT m a
handleErrorTreeT onErr m =
  TreeT <<< map (handleErrorNodeT onErr) $
    catchError (runTreeT m) (runTreeT <<< onErr)

instance monadThrowTreeT :: MonadThrow e m => MonadThrow e (TreeT m) where
  throwError = lift <<< throwError

instance monadErrorTreeT :: MonadError e m => MonadError e (TreeT m) where
  catchError = flip handleErrorTreeT

-- PureScript does not have this type class
-- instance MonadResource m => MonadResource (TreeT m) where
--   liftResourceT =
--     lift <<< liftResourceT

------------------------------------------------------------------------
-- Show/Show1 instances

instance showNodeT :: (Show (m a), Show a) => Show (NodeT m a) where
  show = ?notSure

instance showTreeT :: (Show (m a), Show a) => Show (TreeT m a) where
  show = ?notSure2

-- PureScript does not have these type classes
-- instance Show1 m => Show1 (NodeT m) where
-- | liftShowsPrec sp sl d (NodeT x xs) =
-- |   let
-- |     sp1 =
-- |       liftShowsPrec sp sl
-- |
-- |     sl1 =
-- |       liftShowList sp sl
-- |
-- |     sp2 =
-- |       liftShowsPrec sp1 sl1
-- |   in
-- |     showsBinaryWith sp sp2 "NodeT" d x xs
-- instance Show1 m => Show1 (TreeT m) where
-- | liftShowsPrec sp sl d (TreeT m) =
-- |   let
-- |     sp1 =
-- |       liftShowsPrec sp sl
-- |
-- |     sl1 =
-- |       liftShowList sp sl
-- |
-- |     sp2 =
-- |       liftShowsPrec sp1 sl1
-- |   in
-- |     showsUnaryWith sp2 "TreeT" d m

------------------------------------------------------------------------
-- Pretty Printing

-- |
-- Rendering implementation based on the one from containers/Data.Tree

renderTreeTLines :: forall m. Monad m => TreeT m String -> m (List String)
renderTreeTLines (TreeT m) = do
  NodeT x xs0 <- m
  xs <- renderForestLines xs0
  pure $
    lines (renderNodeT x) <|> xs

renderNodeT :: String -> String
renderNodeT str
  | length str == 1 = " " <> str
  | otherwise = str

renderForestLines :: forall m. Monad m => List (TreeT m String) -> m (List String)
renderForestLines xs0 =
  let
    shift hd other =
      zipWith (++) (hd : repeat other)
  in
    case xs0 of
      Nil ->
        pure Nil

      Cons x Nil -> do
        s <- renderTreeTLines x
        pure $
shift " └╼" "   " s

      Cons x xs -> do
        s <- renderTreeTLines x
        ss <- renderForestLines xs
        pure $
shift " ├╼" " │ " s ++ ss

-- | Render a tree of strings.
render :: Tree String -> String
render = runIdentity <<< renderT

-- | Render a tree of strings, note that this forces all the delayed effects in
-- | the tree.
renderT :: forall m. Monad m => TreeT m String -> m String
renderT =
  map unlines <<< renderTreeTLines
