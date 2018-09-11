module Lab1 where
import Data.List
import Test.QuickCheck

-- time taken 01:30
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

--if well designed, guilty should give a singleton list, so not all liars,
--but just the thief.

--We assume all boys know who did it. Therefore, if they were all honest, we would
--expect 5 accusers for a particular person. Since three people are honest,
--we'll definitely see 3 accusations pointing to a particular person.
--then the two liars will point to someone else.

--Luckily, only one person has three accusers. We could imagine that the three honest people
--would point to two people and still be honest. This would give two possible guilty people
--and wouldn't let us pick a cuplrit, even though we know who the three honest people are.
guilty, honest :: [Boy]
guilty = [boy | boy <- boys, (length (accusers boy) == 3)]

honest = accusers (guilty !! 0)
