module PerformanceEx1 where

import Lecture5

runPerformanceTest1 = do
                        putStrLn "-- == Running Lecture5 solver == --"
                        let x = all (solved . head) [solveNs (initNode example1) | _ <- [1..1000]]
                        print x