module DiffLib (
    diff,
    DiffSource (..)
) where 

import Data.Array.IArray
import Data.List

{-
  Learning: !? only returns Nothing for out of bounds access
  Fixed: 1000 will be problematic when input list is large
  --> used maxBound instead
-}



wrapSE :: [a] -> [DiffEntry a]
wrapSE as = DEStart : ((map (DEValue) as) ++ [DEEnd])

data DiffEntry a = DEStart | DEValue a | DEEnd deriving (Eq, Show)

comparisonTable :: (Eq a) => [a] -> [a] -> [[Bool]]
comparisonTable a b = map (\x -> map (x ==) a) b

comparisonArray :: (Eq a) => [DiffEntry a] -> [DiffEntry a] -> Array (Int, Int) Bool
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
        nextPos (0, y) = [(0, pred y)]
        nextPos (x, 0) = [(pred x, 0)]
        nextPos (x, y) = case (compAr ! (x , y)) of 
            True  -> [(pred x, pred y), (pred x, y), (x, pred y)]
            False -> [                  (pred x, y), (x, pred y)]

scorer :: (Array (Int, Int) Bool, Array (Int, Int) Int, [((Int, Int), Int)]) -> Maybe (((Int, Int), Int), (Array (Int, Int) Bool, Array (Int, Int) Int, [((Int, Int), Int)]))
scorer (_, _, []) = Nothing
scorer (compAr, scoreAr, ins) = Just (head ins, (compAr, scoreAr//(head ins: news), (tail ins) ++ news))
    where
        news = nextPosScored compAr scoreAr $ head ins

scoreComparison :: Array (Int, Int) Bool -> Array (Int, Int) Int
scoreComparison arComp = array (bounds arComp) $ unfoldr scorer (arComp, genArray (bounds arComp) (const maxBound), [(snd $ bounds arComp, 0)])

diffResult:: [a] -> [a] -> (Int, Int) -> Array (Int, Int) Int -> [(DiffSource, a)]
diffResult [] _ _ _ = []
diffResult _ [] _ _ = []
diffResult l1 l2 pos score = (d, v) : (diffResult l1' l2' pn score)
    where
        nextScore = pred $ score!pos 
        (pn, d, v, l1', l2') = nextPos pos l1 l2
        nextPos :: (Int, Int) -> [a] -> [a] -> ((Int, Int), DiffSource, a, [a], [a])
        nextPos (x, y) ll1 ll2
            | score!(succ x, y     ) == nextScore = ((succ x,      y), DsLeft,  head ll1, tail ll1, ll2)
            | score!(x,      succ y) == nextScore = ((x     , succ y), DsRight, head ll2, ll1, tail ll2)
            | score!(succ x, succ y) == nextScore = ((succ x, succ y), DsBoth,  head ll2, tail ll1, tail ll2)

data DiffSource = DsLeft | DsRight | DsBoth deriving (Eq, Show)

diff :: (Eq a) => [a] -> [a] -> [(DiffSource, a)]
diff l1 l2 = diffResult l1 l2 (0, 0) $ scoreComparison $ comparisonArray (wrapSE l1) (wrapSE l2)