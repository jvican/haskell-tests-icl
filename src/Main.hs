module Main where

import BDDs

main = do
           print $ buildBDD b1 []
           print $ restrict b2 1 False
           print $ restrict b2 1 True
           print $ buildBDD b2 [1,2]
           print $ buildBDD b3 [1]
           print $ restrict (Or(IdRef 2)(Not(IdRef 3))) 2 False
           print $ restrict (Or(IdRef 2)(Not(IdRef 3))) 2 True
           print $ buildBDD (Or(IdRef 2)(Not(IdRef 3))) [2, 3]
           print $ buildBDD (Not(IdRef 3)) [3]
           print $ buildBDD b4 [7,2,3]
           print $ buildBDD b5 [7,2,3]
           print $ buildBDD b6 [1,2,3,4]
           print $ buildBDD b7 [2,3,9]
           print $ buildBDD b8 [1]
