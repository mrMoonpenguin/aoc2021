main :: IO ()
main = do
  contents <- readFile "input"
  print $ countIncreases $ map readInt (lines contents)

readInt :: String -> Int
readInt = read

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
