module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- == Exercise 2 == --
{-
Time spend: 2 hours
-}
-- 
{-
Scratch implementation

This implementation has a max length of 30 for the set and chooses random integers between -30 and 30.
-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (-n,n))

randomSet :: IO (Set Int) 
randomSet = do
              listLength <- getRandomInt 30
              list  <- sequence [getRandomInt 30 | _ <- [0..abs(listLength)]]
              return (Set (nub list))

-- Quickcheck arbitrary implementation
instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = (\x -> Set x) <$> arbitrary



