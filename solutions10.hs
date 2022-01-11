import Control.Monad (join)
import Data.List (group)
-----------------------------------------------------------
-- 1. Find the last element of a list.
myLast :: [a] -> a
myLast = last
-- | Example:
--
-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
-- 'z'

-----------------------------------------------------------
-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = head . tail . reverse
-- | Example:
--
-- >>> myButLast [1,2,3,4]
-- 3
-- >>> myButLast ['a'..'z']
-- 'y'

-----------------------------------------------------------
-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt = (. subtract 1) . (!!)
-- | Example:
--
-- >>> elementAt [1,2,3] 2
-- 2
-- >>> elementAt "haskell" 5
-- 'e'

-----------------------------------------------------------
-- 4. Find the number of elements of a list.
myLength :: [a] -> Int
myLength = length
-- | Example:
--
-- >>> myLength [123, 456, 789]
-- 3
-- >>> myLength "Hello, world!"
-- 13

-----------------------------------------------------------
-- 5. Reverse a list.
myReverse :: [a] -> [a]
myReverse = reverse
-- | Example:
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]

-----------------------------------------------------------
-- 6. Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward;
-- e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = (==) <$> id <*> reverse
-- | Example:
--
-- >>> isPalindrome [1,2,3]
-- False
-- >>> isPalindrome "madamimadam"
-- True
-- >>> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

-----------------------------------------------------------
-- 7. Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List l) = concatMap flatten l
-- | Example:
--
-- >>> flatten (Elem 5)
-- [5]
-- >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- >>> flatten (List [])
-- []

-----------------------------------------------------------
-- 8. Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress = join $ foldr addIfNotPresent . (:[]) . last
    where addIfNotPresent x acc = if x == head acc then acc else x:acc
-- | Example:
--
-- >>> compress "aaaabccaadeeee"
-- "abcade"

-----------------------------------------------------------
-- 9. Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements,
-- they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack = group
-- | Example:
--
-- >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

-----------------------------------------------------------
-- 10. Run-length encoding of a list. Use the result of problem P09
-- to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) 
-- where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode = map ((,) <$> length <*> head) . pack
-- | Example:
--
-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]


