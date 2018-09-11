module Lab1 where
import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && (not (x && y))

accuses :: Boy -> Boy -> Bool
accuses accuser accused | accuser == Matthew = case () of
                          () | accused == Matthew -> False
                             | accused == Carl    -> False
                             | otherwise          -> True
                        | accuser == Peter   = case () of
                          () | accused == Matthew -> True
                             | accused == Jack    -> True
                             | otherwise          -> False
                        | accuser == Jack         = ((not (accuses Matthew accused)) &&
                                                     (not (accuses Peter   accused)))
                        | accuser == Arnold       = xor (accuses Matthew  accused)
                                                        (accuses Peter    accused)
                        | accuser == Carl         = not (accuses Arnold   accused)

accusers :: Boy -> [Boy]
accusers accused  = [boy | boy <- boys, (accuses boy accused)]

honestcombinations :: [[Boy]]
honestcombinations = [l | l <- subsequences(boys), ((length l) == 3)]

--if well designed, guilty should give a singleton list, so not all liars,
--but just the thief.
guilty, honest :: [Boy]
guilty = [boy | boy <- boys, (any (\y -> y == True) [((accusers boy) == honestboys) | honestboys <- honestcombinations])]

honest = accusers (guilty !! 0)
