module Main (main) where
import MatrixLands
import Control.Exception (assert)
import Data.Array

main :: IO ()
main = do
    let m = MatrixLands $ listArray ((0,0),(0,0)) 
            [1]
    c <- countIslandsWithTrace m
    print $ assert (c == 1) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(1,1))
            [0,1,
             1,0]
    c <- countIslandsWithTrace m
    print $ assert (c == 2) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(1,1))
            [1,1,
             1,1]
    c <- countIslandsWithTrace m
    print $ assert (c == 1) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(2,2))
            [0,1,0,
             1,0,0,
             0,0,1]
    c <- countIslandsWithTrace m
    print $ assert (c == 3) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(1,2))
            [1,0,0,
             0,1,1]
    c <- countIslandsWithTrace m
        
    print $ assert (c == 2) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(2,2))
            [1,1,0,
             0,1,1,
             1,0,1]
    c <- countIslandsWithTrace m
    print $ assert (c == 2) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(2,2))
            [1,1,1,
             0,0,1,
             1,1,1]
    c <- countIslandsWithTrace m
    print $ assert (c == 1) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(3,3))
            [0,0,0,1,
             0,0,0,0,
             0,0,1,0,
             0,0,0,0]
    c <- countIslandsWithTrace m
    print $ assert (c == 2) "ok " ++ show m
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(3,3))
            [1,1,0,1,
             0,1,1,1,
             1,0,0,0,
             1,1,0,1]
    c <- countIslandsWithTrace m
    print $ assert (c == 3) "ok " ++ show m 
    -- -------------------------
    let m = MatrixLands $ listArray ((0,0),(3,3))
            [1,1,0,1,
             1,1,1,1,
             1,0,0,0,
             1,0,0,1]
    c <- countIslandsWithTrace m
    print $ assert (c == 2) "ok " ++ show m
    -- -------------------------
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
            1,1,0,1,0,0,0,1,0,0]
    c <- countIslandsWithTrace m
    print $ assert (c == 9) "ok " ++ show m
    -- -------------------------
