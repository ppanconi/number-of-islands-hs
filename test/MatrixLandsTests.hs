module Main where
import MatrixLands
import Data.Array
import Test.HUnit
import qualified System.Exit as Exit

mat :: ((Integer, Integer), (Integer, Integer)) -> [Integer] -> MatrixLands
mat indxs xs = MatrixLands $ listArray indxs xs

test1 :: Test
test1 = TestCase (do
        let m = mat 
                ((0,0),(0,0)) 
                [1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 1") 1 c)

test2 :: Test
test2 = TestCase (do
        let m = mat 
                ((0,0),(1,1))
                [0,1,
                 1,0]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 2") 2 c)

test3 :: Test
test3 = TestCase (do
        let m = mat 
                ((0,0),(1,1))
                [1,1,
                1,1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 1") 1 c)

test4 :: Test
test4 = TestCase (do
        let m = mat 
                ((0,0),(2,2))
                [0,1,0,
                1,0,0,
                0,0,1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 3") 3 c)

test5 :: Test
test5 = TestCase (do
        let m = mat 
                ((0,0),(1,2))
                [1,0,0,
                 0,1,1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 2") 2 c)

test6 :: Test
test6 = TestCase (do
        let m = mat 
                ((0,0),(2,2))
                [1,1,0,
                 0,1,1,
                 1,0,1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 2") 2 c)

test7 :: Test
test7 = TestCase (do
        let m = mat 
                ((0,0),(2,2)) 
                [1,1,1,
                 0,0,1,
                 1,1,1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 1") 1 c)

test8 :: Test
test8 = TestCase (do
        let m = mat 
                ((0,0),(3,3))
                [0,0,0,1,
                0,0,0,0,
                0,0,1,0,
                0,0,0,0]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 2") 2 c)

test9 :: Test
test9 = TestCase (do
        let m = mat 
                ((0,0),(3,3))
                [1,1,0,1,
                0,1,1,1,
                1,0,0,0,
                1,1,0,1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 3") 3 c)

test10 :: Test
test10 = TestCase (do
        let m = mat 
                ((0,0),(3,3))
                [1,1,0,1,
                 1,1,1,1,
                 1,0,0,0,
                 1,0,0,1]
        c <- countIslandsWithTrace m
        assertEqual (show m ++ " should return 2") 2 c)

test11 :: Test
test11 = TestCase (do
        let m = mat 
                ((0,0),(9,9))
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
        assertEqual (show m ++ " should return 10") 11 c)


tests :: Test
tests = TestList [
        TestLabel "test1" test1,
        TestLabel "test2" test2,
        TestLabel "test3" test3,
        TestLabel "test4" test4,
        TestLabel "test5" test5,
        TestLabel "test6" test6,
        TestLabel "test7" test7,
        TestLabel "test8" test8,
        TestLabel "test9" test9,
        TestLabel "test10" test10,
        TestLabel "test11" test11
 ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess