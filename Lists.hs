import Control.Monad (join)
import Data.Tuple ( swap )
import qualified Control.Arrow as Data.Bifunctor
import Data.Bifunctor (second)
-----------------------------------------------------------
-- 1. Find the last element of a list.
-- | Example:
--
-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
-- 'z'

myLast :: [a] -> a
myLast = last

-----------------------------------------------------------
-- 2. Find the last but one element of a list.
-- | Example:
--
-- >>> myButLast [1,2,3,4]
-- 3
-- >>> myButLast ['a'..'z']
-- 'y'

myButLast :: [a] -> a
myButLast = head . tail . reverse

-----------------------------------------------------------
-- 3. Find the K'th element of a list. The first element in the list is number 1.
-- | Example:
--
-- >>> elementAt [1,2,3] 2
-- 2
-- >>> elementAt "haskell" 5
-- 'e'

elementAt :: [a] -> Int -> a
elementAt = (. subtract 1) . (!!)

-----------------------------------------------------------
-- 4. Find the number of elements of a list.-- | Example:
--
-- >>> myLength [123, 456, 789]
-- 3
-- >>> myLength "Hello, world!"
-- 13

myLength :: [a] -> Int
myLength = length

-----------------------------------------------------------
-- 5. Reverse a list.
-- | Example:
--
-- >>> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- >>> myReverse [1,2,3,4]
-- [4,3,2,1]

myReverse :: [a] -> [a]
myReverse = reverse

-----------------------------------------------------------
-- 6. Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward;
-- e.g. (x a m a x).
-- | Example:
--
-- >>> isPalindrome [1,2,3]
-- False
-- >>> isPalindrome "madamimadam"
-- True
-- >>> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome :: Eq a => [a] -> Bool
isPalindrome = (==) <$> id <*> reverse

-----------------------------------------------------------
-- 7. Flatten a nested list structure.
-- | Example:
--
-- >>> flatten (Elem 5)
-- [5]
-- >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- >>> flatten (List [])
-- []

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List l) = concatMap flatten l

-----------------------------------------------------------
-- 8. Eliminate consecutive duplicates of list elements.
-- | Example:
--
-- >>> compress "aaaabccaadeeee"
-- "abcade"

compress :: Eq a => [a] -> [a]
compress = join $ foldr addIfNotPresent . (:[]) . last
    where addIfNotPresent x acc = if x == head acc then acc else x:acc

-----------------------------------------------------------
-- 9. Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements,
-- they should be placed in separate sublists.
-- | Example:
--
-- >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:xs') : pack xs''
    where (xs', xs'') = span (== x) xs

-----------------------------------------------------------
-- 10. Run-length encoding of a list. Use the result of problem P09
-- to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) 
-- where N is the number of duplicates of the element E.
-- | Example:
--
-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: Eq a => [a] -> [(Int, a)]
encode = map ((,) <$> length <*> head) . pack

-----------------------------------------------------------
-- 11. Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element 
-- has no duplicates it is simply copied into the result list. 
-- Only elements with duplicates are transferred as (N E) lists.
-- | Example:
-- 
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

data Encoding a = Multiple Int a | Single a deriving Show
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map (encode' <$> length <*> head) . pack
    where encode' 1 = Single
          encode' n = Multiple n

-----------------------------------------------------------
-- 12. Decode a run-length encoded list. 
-- Given a run-length code list generated as 
-- specified in problem 11. Construct its uncompressed version.
-- | Example:
--
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

decodeModified :: [Encoding a] -> [a]
decodeModified = concatMap decode'
    where decode' (Single x) = [x]
          decode' (Multiple n x) = replicate n x

-----------------------------------------------------------
-- 13. Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression
-- method directly. I.e. don't explicitly create the sublists 
-- containing the duplicates, as in problem 9, but only count them. 
-- As in problem P11, simplify the result list by replacing the 
-- singleton lists (1 X) by X.
-- | Example:
--
-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect (x:xs) = (encodeSingle . encodeMultiple') xs' : encodeDirect rest
    where (xs',rest) = span (== x) xs
          encodeMultiple' = flip Multiple x . (+1) . length
          encodeSingle (Multiple 1 x) = Single x
          encodeSingle e = e

-----------------------------------------------------------
-- 14. (*) Duplicate the elements of a list.
-- | Example:
--
-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-----------------------------------------------------------
-- 15. Replicate the elements of a list a given number of times.
-- | Example:
--
-- >>> repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli = (. replicate) . (>>=)

-----------------------------------------------------------
-- 16. Drop every N'th element from a list.
-- | Example:
--
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

-----------------------------------------------------------
-- 17. Split a list into two parts; the length of the first part is given.
-- | Example:
--
-- >>> split "abcdefghik" 3
-- ("abc","defghik")
split :: [a] -> Int -> ([a], [a])
split (x:xs) n | n > 0 = (x : first, rest) where (first, rest) = split xs (n - 1)
split xs _             = ([], xs)

-----------------------------------------------------------
-- 18.  Extract a slice from a list.
-- Given two indices, i and k, the slice is the list 
-- containing the elements between the i'th and k'th element
-- of the original list (both limits included). 
-- Start counting the elements with 1.
-- | Example:
--
-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice xs from to = take (to - from + 1) (drop (from - 1) xs)

-----------------------------------------------------------
-- 19. Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-- | Example:
--
-- >>> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- >>> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate xs = uncurry (++) . swap . split xs . (`mod` length xs)

-----------------------------------------------------------
-- 20. Remove the K'th element from a list.
-- | Example:
--
-- >>> removeAt 2 "abcd"
-- ('b',"acd")
-- >>> removeAt 3 "abcd"
-- ('c',"abd")

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (elementAt xs n, uncurry (++) . second tail . split xs $ n - 1)

-----------------------------------------------------------
-- 21. Insert an element at a given position into a list.
-- | Example:
--
-- >>> insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs = uncurry (++) . second (x:) . split xs . subtract 1

-----------------------------------------------------------
-- 22. Create a list containing all integers within a given range.
-- | Example:
--
-- >>> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range = enumFromTo
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
