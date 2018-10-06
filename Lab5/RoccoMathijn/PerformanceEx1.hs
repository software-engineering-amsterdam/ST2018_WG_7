module PerformanceEx1 where

import Exercise1

main = do
        putStrLn "-- == Running Exercise1 solver == --"
        let x = all (solved . head) [solveNs (initNode exercise1) | _ <- [1..100]]
        print x