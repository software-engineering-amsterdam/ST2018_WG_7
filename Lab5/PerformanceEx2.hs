module PerformanceEx2 where

import Lab5_Ex2

runPerformanceTest2 = do
                        putStrLn "-- == Running refactored solver == --"
                        let x = all (solved . head) [solveNs (initNode example1) | _ <- [1..1000]]
                        print x