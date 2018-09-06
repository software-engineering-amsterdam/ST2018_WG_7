module Lab1 where
import Data.List
import Test.QuickCheck

-- Redo Workshop 5, time 00:20
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

permsize ::[a] -> Int
permsize a = length(perms(a))

factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

shorthand :: [a] -> Int
shorthand a = factorial (length(a))

test :: [a] -> Bool
test a = permsize a == shorthand a

-- The property is hard to test timely,
--  since the size of the permutation set grows in a factorial manner.

-- We're testing whether the number of sets returned by perms of a corresponds to
-- the factorial of the cardinality of a. We do not know whether perms actually
-- generates all the unique permutations, and doesn't just create the correct number
-- of sets, but they're all empty.
