module Lab1 where
import Data.List
import Test.QuickCheck

-- Redo Workshop 4, time 00:24

powersetsize :: [a] -> Int
powersetsize a = length(subsequences(a))

shorthand :: [a] -> Int
shorthand a = 2^(length(a))

test :: [a] -> Bool
test a = powersetsize a == shorthand a

-- The property is hard to test in a timely manner,
-- as the powersetsize supposedly grows exponentially.

-- We're testing whether subsequences creates a number of subsets corresponding to
-- |P(A)| = 2^n, where n = |A|. In this test we do not know whether all unique subsets
-- are created, and it's not just creating 2^n empty sets for example.
