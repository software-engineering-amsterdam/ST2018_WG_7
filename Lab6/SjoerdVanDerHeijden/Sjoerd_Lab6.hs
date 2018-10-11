module Sjoerd_Lab6 where

import Data.List
import Test.QuickCheck
import Data.Time.Clock

import Lecture6

-------------------------------------------------------------------------------
-- == Assignment 1 == --
-- exM :: Integer -> Integer -> Integer -> Integer
-- exM x 1 divisor = x `mod` divisor
-- exM x power divisor | mod power 2 == 0 = (exM x (power `div` 2) divisor)^2
--                     | otherwise = exM x (power-1) divisor * x `mod` divisor


-------------------------------------------------------------------------------
-- == Assignment 2 == --
efficiencyTester = do 
                    justnow <- getCurrentTime
                    -- x <- arbitrary
                    -- power <- arbitrary
                    -- divisor <- arbitrary
                    -- exM x power divisor
                    now <- getCurrentTime
                    return (diffUTCTime justnow now)