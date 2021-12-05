import Data.List
import Data.Maybe
import Text.Regex

type Line = ((Int, Int), (Int, Int))

type Grid = [[Int]]

main :: IO ()
main = do
  contents <- readFile "input"
  let input = map ((\(a : b : c : d : _) -> ((readInt a, readInt b), (readInt c, readInt d))) . fromJust . matchRegex (mkRegex "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)")) (lines contents)
  print (countIntersects $ simulate grid input)

readInt :: String -> Int
readInt = read

simulate :: Grid -> [Line] -> Grid
simulate g [] = g
simulate g (((x1, y1), (x2, y2)) : t)
  | y1 == y2 = simulate (replaceAtIndex y1 (incr (g !! y1) [xmin .. xmax]) g) t
  | x1 == x2 = simulate (transpose (replaceAtIndex x1 (incr (g' !! x1) [ymin .. ymax]) g')) t
  | otherwise = simulate g t
  where
    g' = transpose g
    xmax = max x1 x2
    xmin = min x1 x2
    ymax = max y1 y2
    ymin = min y1 y2

incr :: [Int] -> [Int] -> [Int]
incr = foldl' (\l x -> replaceAtIndex x (l !! x + 1) l)

grid :: [[Int]]
grid = replicate 1000 (replicate 1000 0)

countIntersects :: Grid -> Int
countIntersects = sum . map (length . filter (\x -> x /= 0 && x /= 1))

-- https://stackoverflow.com/questions/10133361/haskell-replace-element-in-list/10133429
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b) where (a, _ : b) = splitAt n ls