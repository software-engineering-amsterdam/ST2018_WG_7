module Lab1 where
import Data.List
import Test.QuickCheck

-- time started 00:40
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

func :: Integer -> Integer
func x = product(takeWhile (\ y -> y <= x) primes)+1

counterexamples :: [Integer]
counterexamples = filter (\ x -> (not (prime (func x)))) primes

--The smallest counterexample is the list of primes until 13. 2+3+5+7+11+13+1 = 32
