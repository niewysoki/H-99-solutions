-- 1. Find the last element of a list.
myLast :: [a] -> a
myLast = last

-- | Example:
--
-- >>> myLast [1,2,3,4]
-- 4
-- >>> myLast ['x','y','z']
-- 'z'

-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- | Example:
--
-- >>> myButLast [1,2,3,4]
-- 3
-- >>> myButLast ['a'..'z']
-- 'y'

-- 3. Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt = (. subtract 1) . (!!)

-- | Example:
--
-- >>> elementAt [1,2,3] 2
-- 2
-- >>> elementAt "haskell" 5
-- 'e'





