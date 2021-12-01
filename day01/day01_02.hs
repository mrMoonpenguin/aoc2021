main :: IO ()
main = do
  contents <- readFile "input"
  print $ countIncreases $ cluster 3 $ map readInt (lines contents)

readInt :: String -> Int
readInt = read

cluster :: Int -> [Int] -> [Int]
cluster n l@(h : t) =
  if n <= length l
    then sum (take n l) : cluster n t
    else []

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (h1 : l@(h2 : t)) = go h1 h2 + countIncreases l
  where
    go :: Int -> Int -> Int
    go x y =
      if x < y
        then 1
        else 0