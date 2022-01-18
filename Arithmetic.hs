import Data.List (nub)
import Data.Ratio ()
-----------------------------------------------------------
-- 32. Determine the greatest common divisor of two 
-- positive Integer numbers. Use Euclid's algorithm.
-- | Example:
--
-- >>> myGCD 4 8
-- 4
-- >>> myGCD 7 14
-- 7

myGCD :: Integral a => a -> a -> a
myGCD a b = if a == 0 then b else myGCD (b `mod` a) a

-----------------------------------------------------------
-- 33. Determine whether two positive Int numbers are 
-- coprime. Two numbers are coprime if their greatest 
-- common divisor equals 1.
-- | Example:
--
-- >>> coprime 35 64
-- True

coprime :: Int -> Int -> Bool
coprime = ((== 1) .) . myGCD

-----------------------------------------------------------
-- 34. Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined
-- as the number of positive Ints r (1 <= r < m) that 
-- are coprime to m.
-- | Example:
-- >>> totient 10
-- 4

totient :: Integral a => a -> a
totient = id

-----------------------------------------------------------
-- 35. Determine the prime factors of a given positive 
-- integer. Construct a flat list containing the 
-- prime factors in ascending order.
-- | Example:
-- >>> primeFactors 315
-- [3,3,5,7]

fa

-----------------------------------------------------------
-----------------------------------------------------------

