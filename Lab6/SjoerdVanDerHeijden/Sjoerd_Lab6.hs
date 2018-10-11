module Sjoerd_Lab6 where

import Data.List
import Test.QuickCheck

exM :: Integer -> Integer -> Integer -> Integer
exM x 1 divisor = x `mod` divisor
exM x power divisor | mod power 2 == 0 = (exM x (power `div` 2) divisor)^2
                    | otherwise = exM x (power-1) divisor * x `mod` divisor

-- quicktester :: Integral a => a -> Bool
-- quicktester a = ((mod (a^32) 17) * (mod a 17)) == (mod (a^33) 17)

-- do justnow <- x; now <- getCurrentTime ; return (diffUTCTime justnow now)