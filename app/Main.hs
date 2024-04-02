module Main where

import MatrixLands
import Control.Exception (assert)
import Data.Array

main :: IO ()
main = do
    let m = MatrixLands $ listArray ((0,0),(9,9))
            [1,1,0,1,1,0,0,0,1,0,
            1,1,1,1,1,1,0,0,0,1,
            1,0,0,0,1,1,1,1,0,1,
            1,0,0,1,0,0,0,1,1,0,
            1,0,0,1,0,0,0,1,0,0,
            0,0,1,1,0,0,0,0,1,1,
            0,0,0,1,0,1,0,0,0,1,
            0,0,1,0,0,1,1,1,1,0,
            1,0,0,1,0,0,0,1,0,0,
            1,1,0,1,0,0,0,1,0,1]
    c <- countIslandsWithTrace m
    print $ assert (c == 10) "ok " ++ show m
