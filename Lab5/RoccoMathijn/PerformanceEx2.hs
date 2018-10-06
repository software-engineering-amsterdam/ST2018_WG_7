module PerformanceEx2 where

import Exercise2

main = do
        putStrLn "-- == Running Exercise2 solver == --"
        let x = all (solved . head) [solveNs (initNode exercise2) | _ <- [1..100]]
        print x