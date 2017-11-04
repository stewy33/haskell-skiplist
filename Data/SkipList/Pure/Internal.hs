{-# LANGUAGE ExistentialQuantification #-}

module Data.SkipList.Pure.Internal where

import Data.Monoid
import System.Random
import Control.DeepSeq
import Prelude hiding (lookup)

data Node k v
    = Node { nKey :: k
           , nValue :: v
           , nSibling :: Node k v
           , nChild :: Node k v }
    | Head { nSibling :: Node k v
           , nChild :: Node k v }
    | Nil
    deriving (Show)

data SkipList k v = forall g. RandomGen g =>
                              SkipList
    { skpSeed :: g
    , skpInternal :: Node k v
    } 

null :: SkipList k v -> Bool
null (SkipList _ (Head _ _)) = True
null _ = False

empty :: RandomGen g => g -> SkipList k v
empty seed = SkipList seed $ Head Nil Nil

singleton :: (RandomGen g, Ord k) => k -> v -> g -> SkipList k v
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

numWins :: RandomGen g => g -> (Int, g)
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
      where sN = sibling n

lookup :: Ord k => k -> SkipList k v -> Maybe v
lookup k = go . skpInternal
  where
    go Nil = Nothing
    go n
      | nodeLTKey sN k = go $ nSibling n
      | nodeEQKey sN k = Just $ nValue sN
      | otherwise = go $ nChild n
      where sN = sibling n

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
      where sN = sibling n
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
        where sN = sibling n

delete :: Ord k => k -> SkipList k v -> SkipList k v
delete k (SkipList g i) = SkipList g $ findAndDelete i
  where
    findAndDelete Nil = Nil
    findAndDelete n
      | nodeLTKey sN k = n {nSibling = findAndDelete $ nSibling n}
      | nodeEQKey sN k = deleteNode n {nSibling = Nil} sN
      | otherwise = n {nChild = findAndDelete $ nChild n}
      where sN = sibling n
    deleteNode Nil _ = Nil
    deleteNode start toDelete = (go start) {nChild = deleteNode (nChild start) $ nChild toDelete}
      where
        go Nil = nSibling toDelete
        go n = n {nSibling = go $ nSibling n}

fromList :: (RandomGen g, Ord k) => g -> [(k, v)] -> SkipList k v
fromList = foldr (\(k, v) skp -> insert k v skp) . empty

toList :: SkipList k v -> [(k, v)]
toList skp = go (skpInternal skp) []
  where
    go Nil = id
    go (Head s c) = go s . go c
    go (Node k v s c) = go s . go c . ((k, v) :)


instance (Show k, Show v) => Show (SkipList k v) where
    show (SkipList _ i) = "SkipList {\n" ++ "\n\tskpInternal = \n\t" ++ show i ++ "\n}"

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

instance (NFData k, NFData v) => NFData (SkipList k v) where
    rnf (SkipList _ i) = rnf i

instance (NFData k, NFData v) => NFData (Node k v) where
    rnf Nil = ()
    rnf (Head s c) = rnf s `seq` rnf c
    rnf (Node k v s c) = rnf k `seq` rnf v `seq` rnf s `seq` rnf c
