main :: IO ()
main = do
  contents <- readFile "input"
  print $ (\(x, y, _) -> x * y) $ foldr (go . (\l -> let (s : i : _) = words l in (s, readInt i))) (0, 0, 0) (reverse $ lines contents)
  where
    go :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
    go ("forward", n) (x, y, a) = (x + n, y + n * a, a)
    go ("down", n) (x, y, a) = (x, y, a + n)
    go ("up", n) (x, y, a) = (x, y, a - n)

readInt :: String -> Int
readInt = read