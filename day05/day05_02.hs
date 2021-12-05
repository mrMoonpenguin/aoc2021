import Data.List (foldl', transpose)
import Data.Map (Map, adjust, empty, filter, insert, lookup, size)
import Data.Maybe (fromJust, isJust)
import Text.Regex (matchRegex, mkRegex)

type Line = ((Int, Int), (Int, Int))

type Grid = Map (Int, Int) Int

main :: IO ()
main = do
  contents <- readFile "input"
  let input = map ((\(a : b : c : d : _) -> ((readInt a + 1, readInt b + 1), (readInt c + 1, readInt d + 1))) . fromJust . matchRegex (mkRegex "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)")) (lines contents)
  print (countIntersects $ simulate input empty)

readInt :: String -> Int
readInt = read

simulate :: [Line] -> Grid -> Grid
simulate [] m = m
simulate ((c1@(x1, y1), c2@(x2, y2)) : t) m = simulate t (incr coords m)
  where
    coords = getCoords c1 c2

getCoords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getCoords (x1, y1) (x2, y2)
  | x1 == x2 = zip (replicate (length ys) x1) ys
  | y1 == y2 = zip xs (replicate (length xs) y1)
  | otherwise = zip xs ys
  where
    xs = if x1 < x2 then [x1 .. x2] else [x1, x1 - 1 .. x2]
    ys = if y1 < y2 then [y1 .. y2] else [y1, y1 - 1 .. y2]

incr :: [(Int, Int)] -> Grid -> Grid
incr [] m = m
incr (c@(x, y) : cs) m = incr cs m'
  where
    el = Data.Map.lookup c m
    m' = if isJust el then adjust (+ 1) c m else insert c 1 m

countIntersects :: Grid -> Int
countIntersects = size . Data.Map.filter (> 1)