{-# LANGUAGE ExistentialQuantification #-}

module SkipList where

import Data.Monoid
import System.Random

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
    , skpHeight :: Int
    , skpInternal :: Node k v
    }

instance (Show k, Show v) => Show (SkipList k v) where
    show (SkipList _ h i) =
        "SkipList {\n" ++ "\tskpHeight = " ++ show h ++ "\n\tskpInternal = \n\t" ++ show i ++ "\n}"

instance Foldable (SkipList k) where
    foldMap f skp = go $ skpInternal skp
      where
        go (Node _ v s c) = f v <> go c <> go s
        go Nil = mempty
        go (Head s c) = go c <> go s

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

nodeEQKey :: Eq k => Node k v -> k -> Bool
nodeEQKey (Node k _ _ _) key = k == key
nodeEQKey _ _ = False

numWins :: RandomGen g => g -> (Int, g)
numWins = go 0
  where
    go n gen =
        let (b, g) = random gen
        in if b
               then go (succ n) g
               else (n, g)

addLevels :: Ord k => k -> v -> Int -> SkipList k v -> SkipList k v
addLevels k v numLevels skp@(SkipList g h i)
    | numLevels <= 0 = skp
    | otherwise = go numLevels $ splitLevel i
  where
    -- when there is one level to be added, joins two lineages by a sibling
    -- relationship
    go 1 (l, r) = SkipList g (h + numLevels) (Head (Node k v Nil r) l)
    go levelsLeft (l, r) = go (pred levelsLeft) (Head Nil l, Head Nil r)

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
member k = memberNode . skpInternal
  where
    memberNode Nil = False
    memberNode n
      | nodeEQKey n k = True
      | nodeLTEKey (sibling n) k = memberNode $ nSibling n
      | otherwise = memberNode $ nChild n

insert :: Ord k => k -> v -> SkipList k v -> SkipList k v
insert k v (SkipList g h i) =
    let (i', l, g') = insertSiblings i
    in addLevels k v l $ SkipList g' h i'
  where
    insertSiblings n
        | nodeEQKey n k = (n {nValue = v}, -1, g)
        | nodeLTEKey (sibling n) k =
            let (s, l, g') = insertSiblings $ nSibling n
            in (n {nSibling = s}, l, g')
        | otherwise = insertAfter n . insertChildren $ child n
    insertChildren Nil =
        let (r, g') = numWins g
        in (Nil, r, g')
    insertChildren n = insertSiblings n
    insertAfter n (c, l, g')
        | l < 0 = (n {nChild = c}, -1, g')
        | l == 0 = (n {nSibling = Node k v (nSibling n) c}, pred l, g')
        | otherwise = (n {nSibling = Head (nSibling n) c}, pred l, g')

delete :: Ord k => k -> SkipList k v -> SkipList k v
delete k (SkipList g h i) = SkipList g h $ findAndDelete i
  where
    findAndDelete Nil = Nil
    findAndDelete n
      | nodeEQKey (sibling n) k = n {nSibling = sibling $ sibling n, nChild = deleteNode (nChild n) (child $ sibling n)}
      | nodeLTEKey (sibling n) k = n {nSibling = findAndDelete $ nSibling n}
      | otherwise = n {nChild = findAndDelete $ nChild n}

    deleteNode Nil _ = Nil
    deleteNode start toDelete = (go start) {nChild = deleteNode (nChild start) $ nChild toDelete}
        where
            go Nil = nSibling toDelete
            go n = n {nSibling = go $ nSibling n}

empty :: RandomGen g => g -> SkipList k v
empty seed = SkipList seed 0 (Head Nil Nil)

singleton :: (RandomGen g, Ord k) => k -> v -> g -> SkipList k v
singleton k v = insert k v . empty
