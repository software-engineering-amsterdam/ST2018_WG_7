module Lab1 where
import Data.List
import Test.QuickCheck

-- time 00:30
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

prime101 :: Integer
prime101 = func primes

func :: [Integer] -> Integer
func (x:xs) | prime (x + sum(take 100 xs)) = (x + sum(take 100 xs))
            | otherwise                    = (func xs)
