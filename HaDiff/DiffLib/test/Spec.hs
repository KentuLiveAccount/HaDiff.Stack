import DiffLib

printDiff :: (Show a) => (DiffSource, a) -> IO ()
printDiff (DsLeft, v) = putStrLn $ unwords ["<", (show v)]
printDiff (DsRight, v) = putStrLn $ unwords [">", (show v)]
printDiff (DsBoth, v) = putStrLn $ unwords ["-", (show v)]

source1 = [1, 2, 3, 4, 5]
source2 = [1, 7, 2, 3, 9, 5]

main :: IO ()
main = do 
    print $ concatMap (\y -> (map (\x -> (x, y)) [0..(length source1)])) [0..(length source2)]
    mapM_ printDiff $ diff source1 source2