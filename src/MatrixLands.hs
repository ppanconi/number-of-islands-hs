module MatrixLands where
    import Data.Array
    import VisitableLands
    import SetVisitLogbook
    import Control.Monad.State
    import qualified Data.Set as S
    import Data.Foldable

    newtype MatrixLands = MatrixLands {matrix :: Array (Integer, Integer) Integer} deriving Show

    matrixIsLand :: MatrixLands -> (Integer, Integer) -> Bool
    matrixIsLand l k = matrix l ! k == 1
    matrixNeighboringDistricts :: MatrixLands -> (Integer, Integer) -> [(Integer, Integer)]
    matrixNeighboringDistricts l (i,j) =
        [ (i+1, j) | inRange (bounds (matrix l)) (i+1, j) ] ++
        [ (i, j+1) | inRange (bounds (matrix l)) (i, j+1) ] ++
        [ (i-1, j) | inRange (bounds (matrix l)) (i-1, j) ] ++
        [ (i, j-1) | inRange (bounds (matrix l)) (i, j-1) ]

    instance VisitableLands MatrixLands (Integer, Integer) where
      neighboringDistricts :: MatrixLands -> (Integer, Integer) -> [(Integer, Integer)]
      neighboringDistricts l (i,j) = matrixNeighboringDistricts l (i,j)
      isLand = matrixIsLand

    searchMatrix :: MatrixLands -> State (SetVisitLogbook (Integer, Integer)) Int
    searchMatrix m = searchLands m $ head (indices (matrix m))

    initialLogbook :: SetVisitLogbook (Integer, Integer)
    initialLogbook = SetVisitLogbook S.empty S.empty []

    countIslandsWithTrace :: MatrixLands -> IO Int
    countIslandsWithTrace m =
        let (c, SetVisitLogbook{trace}) = runState (searchMatrix m) initialLogbook
        in do
            print "-----------------------"
            print $ "counted " ++ show c ++ " islands"
            print $ "visited " ++ show (length trace) ++ " districts:"
            let traceStr = foldr' (\t s -> s ++ ">" ++ show t) "" trace
            print traceStr
            return c