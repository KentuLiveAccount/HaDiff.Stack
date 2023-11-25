import DiffLib
import Data.Array.IArray
import Data.List

{-
  Learning: !? only returns Nothing for out of bounds access
-}

source1 = [1, 2, 3, 4, 5]
source2 = [1, 7, 2, 3, 9, 5]

wrapSE as = 0 : (as ++ [100])

comparisonTable :: (Eq a) => [a] -> [a] -> [[Bool]]
comparisonTable a b = map (\x -> map (x ==) a) b

printTable = mapM_ (print)

comparisonArray :: (Eq a) => [a] -> [a] -> Array (Int, Int) Bool
comparisonArray a b = array ((0, 0), (la, lb)) $ zip s2 $ concat $ comparisonTable a b
    where
        la = (length a) - 1
        lb = (length b) - 1
        s2 = concatMap (\y -> (map (\x -> (x, y)) [0..la])) [0..lb]


nextPosScored :: Array (Int, Int) Bool -> Array (Int, Int) Int -> ((Int, Int), Int) -> [((Int, Int), Int)]
nextPosScored compAr scoreAr ((x, y), sc) = filter noOrLess $ map (\p -> (p, sc + 1)) $ nextPos (x, y)
    where
        noOrLess ((x, y), v) = v < (scoreAr ! (x, y))
        nextPos ::  (Int, Int) -> [(Int, Int)]
        nextPos (0, 0) = []
        nextPos (0, y) = [(0, y - 1)]
        nextPos (x, 0) = [(x - 1, 0)]
        nextPos (x, y) = case (compAr ! (x , y)) of 
            True -> [(x - 1, y - 1), (x - 1, y), (x, y - 1)]
            False -> [(x - 1, y), (x, y - 1)]

scorer :: (Array (Int, Int) Bool, Array (Int, Int) Int, [((Int, Int), Int)]) -> Maybe (((Int, Int), Int), (Array (Int, Int) Bool, Array (Int, Int) Int, [((Int, Int), Int)]))
scorer (_, _, []) = Nothing
scorer (compAr, scoreAr, ins) = Just (head ins, (compAr, scoreAr//(head ins: news), (tail ins) ++ news))
    where
        news = nextPosScored compAr scoreAr $ head ins

scoreComparisonList :: Array (Int, Int) Bool -> [((Int, Int), Int)] 
scoreComparisonList arComp = unfoldr scorer (arComp, genArray (bounds arComp) (const 1000), [(snd $ bounds arComp, 0)])

scoreComparison :: Array (Int, Int) Bool -> Array (Int, Int) Int
scoreComparison arComp = array (bounds arComp) $ unfoldr scorer (arComp, genArray (bounds arComp) (const 1000), [(snd $ bounds arComp, 0)])

diffResult:: [a] -> [a] -> (Int, Int) -> Array (Int, Int) Int -> [(Int, a)]
diffResult [] _ _ _ = []
diffResult _ [] _ _ = []
diffResult l1 l2 pos score = (d, v) : (diffResult l1' l2' pn score)
    where
        cur = score!pos
        (pn, d, v, l1', l2') = nextPos pos l1 l2
        nextPos :: (Int, Int) -> [a] -> [a] -> ((Int, Int), Int, a, [a], [a])
        nextPos (x, y) ll1 ll2
            | score!(x + 1, y    ) == (cur - 1) = ((x + 1, y), 0, head ll1, tail ll1, ll2)
            | score!(x,     y + 1) == (cur - 1) = ((x, y + 1), 1, head ll2, ll1, tail ll2)
            | score!(x + 1, y + 1) == (cur - 1) = ((x + 1, y + 1), 2, head ll2, tail ll1, tail ll2)

printDiff :: (Show a) => (Int, a) -> IO ()
printDiff (0, v) = putStrLn $ unwords ["<", (show v)]
printDiff (1, v) = putStrLn $ unwords [">", (show v)]
printDiff (2, v) = putStrLn $ unwords ["-", (show v)]

main :: IO ()
main = do 
    print $ concatMap (\y -> (map (\x -> (x, y)) [0..(length source1)])) [0..(length source2)]
    printTable $ map (\x -> map (\xx -> if xx then 1 else 0) x) $ comparisonTable (wrapSE source1) (wrapSE source2)
    print $ comparisonArray (wrapSE source1) (wrapSE source2)
    print $ scoreComparisonList $ comparisonArray (wrapSE source1) (wrapSE source2)
    print $ elems $ scoreComparison $ comparisonArray (wrapSE source1) (wrapSE source2) -- this prints out transposed matrix serialized.
    mapM_ printDiff $ diffResult source1 source2 (0, 0) $ scoreComparison $ comparisonArray (wrapSE source1) (wrapSE source2)