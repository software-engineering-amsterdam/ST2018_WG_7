module Workshop6 where
import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
    (Leaf "Turing, Alan"))
    (Leaf "Goedel, Kurt")

leafC :: Blt a -> Int
leafC (Leaf l) = 1
leafC (Node l1 l2) = leafC l1 + leafC l2

mapB :: (a -> b) -> Blt a -> Blt b
mapB func (Leaf l) = Leaf (func l)
mapB func (Node l1 l2) = Node (mapB func l1) (mapB func l2)

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

count :: Tree a -> Int
count (T a children) = 1 + sum (map count children)

depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1
