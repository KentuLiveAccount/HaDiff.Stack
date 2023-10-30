import DiffLib
import Data.Array.IArray
import Data.List

{-
  Learning: !? only returns Nothing for out of bounds access
-}

source1 = [1, 2, 3, 4, 5]
source2 = [1, 2, 3, 9, 5]

wrapSE as = 0 : (as ++ [100])

comparisonTable :: (Eq a) => [a] -> [a] -> [[Bool]]
comparisonTable a b = map (\x -> map (x ==) a) b

printTable = mapM_ (print)

comparisonArray :: (Eq a) => [a] -> [a] -> Array (Int, Int) Bool
comparisonArray a b = array ((0, 0), (la, lb)) $ zip s2 $ concat $ comparisonTable a b
    where
        la = (length a) - 1
        lb = (length b) - 1
        s1 = [0..la]
        s2 = concatMap (\x -> map (\y -> (x, y)) [0..lb]) s1

nextPos ::  Array (Int, Int) Bool -> (Int, Int) -> [(Int, Int)]
nextPos _ (0, 0) = []
nextPos _ (0, y) = [(0, y - 1)]
nextPos _ (x, 0) = [(x - 1, 0)]
nextPos arSc (x, y) = case arSc ! (x - 1, y - 1) of
    True -> [(x - 1, y - 1), (x - 1, y), (x, y - 1)] -- order matters
    False -> [(x - 1, y), (x, y - 1)]

nextPosScored compAr scoreAr ((x, y), sc) = filter noOrLess $ map (\p -> (p, sc + 1)) $ nextPos compAr (x, y)
    where
        noOrLess ((x, y), v) = v < (scoreAr ! (x, y))

scorer :: (Array (Int, Int) Bool, Array (Int, Int) Int, [((Int, Int), Int)]) -> Maybe (((Int, Int), Int), (Array (Int, Int) Bool, Array (Int, Int) Int, [((Int, Int), Int)]))
scorer (_, _, []) = Nothing
scorer (compAr, scoreAr, ins) = Just (head ins, (compAr, scoreAr//(head ins: news), (tail ins) ++ news))
    where
        news = nextPosScored compAr scoreAr $ head ins

scoreComparison :: Array (Int, Int) Bool -> Array (Int, Int) Int
scoreComparison arComp = array bd $ unfoldr scorer (arComp, genArray bd (const 1000), [(mx, 0)])
    where
        bd@(_, mx) = bounds arComp



arEmpty :: Array (Int, Int) Int
arEmpty = genArray ((0, 0), (6, 6)) (const 1000)

main :: IO ()
main = do 
    printTable $ comparisonTable (wrapSE source1) (wrapSE source2)
    print $ comparisonArray (wrapSE source1) (wrapSE source2)
    print $ nextPosScored (comparisonArray (wrapSE source1) (wrapSE source2)) arEmpty ((6, 6), 0)
    print $ elems $ scoreComparison $ comparisonArray (wrapSE source1) (wrapSE source2)
