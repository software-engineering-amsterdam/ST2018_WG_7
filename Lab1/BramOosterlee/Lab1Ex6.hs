module Lab1 where
import Data.List
import Test.QuickCheck

-- time started 00:40
prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..]

func :: Int -> Int
func n = product(take n primes)+1

counterexamples :: [Int]
counterexamples = filter (\ n -> (not (prime (func n)))) [1..]

--The smallest counterexample is the list of primes until 13. 2+3+5+7+11+13+1 = 32
