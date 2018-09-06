module Lab1 where
import Test.QuickCheck

--Redo Workshop 2, time: 01:52
sum2 :: Integer -> Integer
sum2 n = sum (map (^2) [1..n])

shorthand2 :: Integer -> Integer
shorthand2 n = n*(n+1)*(2*n+1) `div` 6

test :: (Positive Integer) -> Bool
test (Positive n) = sum2 n == shorthand2 n

--Redo Workshop 3, time: 00:03
sum3 :: Integer -> Integer
sum3 n = sum (map (^3) [1..n])

shorthand3 :: Integer -> Integer
shorthand3 n = (n*(n+1) `div` 2)^2

test2 :: (Positive Integer) -> Bool
test2 (Positive n) = sum3 n == shorthand3 n
