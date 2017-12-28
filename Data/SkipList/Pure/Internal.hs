{-# LANGUAGE ExistentialQuantification #-}

module Data.SkipList.Pure.Internal where

import Control.Applicative (liftA2, liftA3)
import Control.DeepSeq (NFData(..), rnf)
import Data.Monoid ((<>))
import Prelude hiding (lookup)
import System.Random (StdGen, mkStdGen, random)

data Node k v
  = Node { nKey :: k
         , nValue :: v
         , nSibling :: Node k v
         , nChild :: Node k v }
  | Head { nSibling :: Node k v
         , nChild :: Node k v }
  | Nil
  deriving (Eq, Show)

data SkipList k v = SkipList
  { skpSeed :: StdGen
  , skpInternal :: Node k v
  } deriving (Show)

null :: SkipList k v -> Bool
null (SkipList _ (Head Nil Nil)) = True
null _ = False

empty :: StdGen -> SkipList k v
empty seed = SkipList seed $ Head Nil Nil

singleton :: Ord k => k -> v -> StdGen -> SkipList k v
singleton k v = insert k v . empty

-- used to prevent exceptions when calling nSibling or nChild on Nil Nodes
sibling, child :: Node k v -> Node k v
sibling Nil = Nil
sibling n = nSibling n

child Nil = Nil
child n = nChild n

-- Nil is considered greater, Head is considered less than
nodeLTEKey :: Ord k => Node k v -> k -> Bool
nodeLTEKey (Node k _ _ _) key = k <= key
nodeLTEKey Nil _ = False
nodeLTEKey _ _ = True

nodeLTKey :: Ord k => Node k v -> k -> Bool
nodeLTKey (Node k _ _ _) key = k < key
nodeLTKey Nil _ = False
nodeLTKey _ _ = True

nodeEQKey :: Eq k => Node k v -> k -> Bool
nodeEQKey (Node k _ _ _) key = k == key
nodeEQKey _ _ = False

numWins :: StdGen -> (Int, StdGen)
numWins = go 0
  where
    go n gen =
      let (b, g) = random gen
      in if b
           then go (n + 1) g
           else (n, g)

addLevels :: Ord k => k -> v -> (Node k v, Int) -> Node k v
addLevels k v (internal, levelsToAdd)
    -- when there is one level to be added, joins two lineages by a sibling
    -- relationship
  | levelsToAdd > 0 = go levelsToAdd $ splitLevel internal
  | otherwise = internal
  where
    go 1 (l, r) = Head (Node k v Nil r) l
    go levelsLeft (l, r) = go (levelsLeft - 1) (Head Nil l, Head Nil r)
    {- on the top level of the list, if there are more levels to be added,
       there will be a second Head node somewhere to the right of the real
       head. This second head node will be a descendent of the newly
       inserted node after all the levels are added -}
    splitLevel node =
      case nSibling node of
        Head s c -> (node {nSibling = Nil}, Head s c)
        nS ->
          let (next, r) = splitLevel nS
          in (node {nSibling = next}, r)

member :: Ord k => k -> SkipList k v -> Bool
member k = go . skpInternal
  where
    go Nil = False
    go n
      | nodeLTKey sN k = go $ nSibling n
      | nodeEQKey sN k = True
      | otherwise = go $ nChild n
      where
        sN = sibling n

lookup :: Ord k => k -> SkipList k v -> Maybe v
lookup k = go . skpInternal
  where
    go Nil = Nothing
    go n
      | nodeLTKey sN k = go sN
      | nodeEQKey sN k = Just $ nValue sN
      | otherwise = go $ nChild n
      where
        sN = sibling n

insert :: Ord k => k -> v -> SkipList k v -> SkipList k v
insert k v (SkipList g i) = SkipList g' . addLevels k v $ go i
  where
    go Nil = (Nil, wins)
    go n
      | nodeLTKey sN k = (n {nSibling = fst $ go sN}, -1)
      | nodeEQKey sN k = (n {nSibling = sN {nValue = v}}, -1)
      | w < 0 = (n {nChild = cN}, -1)
      | w == 0 = (n {nSibling = Node k v (nSibling n) cN}, -1)
      | otherwise = (n {nSibling = Head (nSibling n) cN}, w - 1)
      where
        sN = sibling n
        (cN, w) = go $ nChild n
    (wins, g') = numWins g

adjust :: Ord k => (v -> v) -> k -> SkipList k v -> SkipList k v
adjust f k (SkipList g i) = SkipList g $ go i
  where
    go Nil = Nil
    go n
      | nodeLTKey sN k = n {nSibling = go sN}
      | nodeEQKey sN k = n {nSibling = sN {nValue = f $ nValue sN}}
      | otherwise = n {nChild = go $ nChild n}
      where
        sN = sibling n

delete :: Ord k => k -> SkipList k v -> SkipList k v
delete k (SkipList g i) = SkipList g $ findAndDelete i
  where
    findAndDelete Nil = Nil
    findAndDelete n
      | nodeLTKey sN k = n {nSibling = findAndDelete $ nSibling n}
      | nodeEQKey sN k = deleteNode n {nSibling = Nil} sN
      | otherwise = n {nChild = findAndDelete $ nChild n}
      where
        sN = sibling n
    deleteNode Nil _ = Nil
    deleteNode start toDelete =
      (go start) {nChild = deleteNode (nChild start) $ nChild toDelete}
      where
        go Nil = nSibling toDelete
        go n = n {nSibling = go $ nSibling n}

fromList :: Ord k => StdGen -> [(k, v)] -> SkipList k v
fromList = foldr (\(k, v) skp -> insert k v skp) . empty

toList :: SkipList k v -> [(k, v)]
toList skp = go (skpInternal skp) []
  where
    go Nil = id
    go (Head s c) = go s . go c
    go (Node k v s c) = go s . go c . ((k, v) :)

instance (Eq k, Eq v) => Eq (SkipList k v) where
  skp1 == skp2 = toList skp1 == toList skp2

instance (Ord k, Ord v) => Ord (SkipList k v) where
  compare skp1 skp2 = compare (toList skp1) (toList skp2)

instance Ord k => Monoid (SkipList k v) where
  mempty = empty $ mkStdGen 0
  -- left-biased mappend
  mappend m1 m2 = foldr (\(k, v) m -> insert k v m) m2 $ toList m1

instance Functor (SkipList k) where
  fmap f skp = skp {skpInternal = go $ skpInternal skp}
    where
      go Nil = Nil
      go (Head s c) = Head (go s) $ go c
      go (Node k v s c) = Node k (f v) (go s) $ go c

instance Foldable (SkipList k) where
  foldMap f skp = go $ skpInternal skp
    where
      go Nil = mempty
      go (Head s c) = go c <> go s
      go (Node _ v s c) = f v <> go c <> go s

instance Traversable (SkipList k) where
  sequenceA (SkipList g i) = SkipList g <$> sequenceA' i
    where
      sequenceA' Nil = pure Nil
      sequenceA' (Head s c) = liftA2 Head (sequenceA' c) $ sequenceA' s
      sequenceA' (Node k v s c) =
        liftA3 (Node k) v (sequenceA' c) $ sequenceA' s

instance (NFData k, NFData v) => NFData (SkipList k v) where
  rnf = rnf . skpInternal

instance (NFData k, NFData v) => NFData (Node k v) where
  rnf Nil = ()
  rnf (Head s c) = rnf s `seq` rnf c
  rnf (Node k v s c) = rnf k `seq` rnf v `seq` rnf s `seq` rnf c
