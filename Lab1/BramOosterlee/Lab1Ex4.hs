module Lab1 where
import Data.List
import Test.QuickCheck

-- Redo Workshop 5, time 00:00 start time 1216
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

reversalprime :: Integer -> Bool
reversalprime n = prime n && prime (reversal n)

first10k :: [Integer]
first10k = filter reversalprime [2..10000]
