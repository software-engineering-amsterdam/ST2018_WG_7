module Sjoerd_Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd


-------------------------------------------------------------------------------
-- == Assignment 2: QuickCheck tester for sets == --
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = arbitrarySet

arbitrarySet :: Arbitrary a => Gen (Set a)
arbitrarySet = do
            x <- arbitrary
            return (Set x)

-- Time: 30 

-------------------------------------------------------------------------------
-- == Assignment 3: QuickCheck tester for sets == --
