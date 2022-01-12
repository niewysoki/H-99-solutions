import Control.Applicative ( Applicative(liftA2) )
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-----------------------------------------------------------
-- 55. Construct completely balanced binary trees
-- In a completely balanced binary tree, the following property
-- holds for every node: The number of nodes in its left subtree 
-- and the number of nodes in its right subtree are almost equal,
-- which means their difference is not greater than one.
-- Write a function cbal-tree to construct completely balanced 
-- binary trees for a given number of nodes. The predicate should 
-- generate all solutions via backtracking. 
-- Put the letter 'x' as information into all nodes of the tree.
cbalTree :: Int -> [Tree Char]
cbalTree n
    | n == 0         = [Empty]
    | n == 1         = [leaf 'x']
    | n `mod` 2 == 1 = map tree $ combine smalls smalls
    | otherwise      = map tree $ combine smalls bigs ++ combine bigs smalls
    where
        smalls = cbalTree ((n - 1) `div` 2)
        bigs = cbalTree (n `div` 2)
        tree = uncurry $ Branch 'x'
        combine = liftA2 (,)

-- | Example
--
-- >>> cbalTree 4
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)]

-----------------------------------------------------------
-- 56. Symmetric binary trees
-- Let us call a binary tree symmetric if you can draw a 
-- vertical line through the root node and then the right subtree 
-- is the mirror image of the left subtree. Write a predicate 
-- symmetric/1 to check whether a given binary tree is symmetric. 
-- Hint: Write a predicate mirror/2 first to check whether one 
-- tree is the mirror image of another. We are only interested 
-- in the structure, not in the contents of the nodes.
mirror :: Tree a -> Tree b -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 l2 && mirror r1 r2
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

-- >>> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
-- False
-- >>> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
-- True

-----------------------------------------------------------
-- 57. Binary search trees (dictionaries)

