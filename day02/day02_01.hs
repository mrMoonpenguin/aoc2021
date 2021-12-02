main :: IO ()
main = do
  contents <- readFile "input"
  print $ uncurry (*) $ foldr (go . (\l -> let (s : i : _) = words l in (s, readInt i))) (0, 0) (lines contents)
  where
    go :: (String, Int) -> (Int, Int) -> (Int, Int)
    go ("forward", n) (x, y) = (x + n, y)
    go ("down", n) (x, y) = (x, y + n)
    go ("up", n) (x, y) = (x, y - n)

readInt :: String -> Int
readInt = read