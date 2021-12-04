import Data.List
import Data.List.Split
import GHC.Data.Maybe

type Board = [[(Int, Bool)]]

main :: IO ()
main = do
  contents <- readFile "input"
  let (rawInput : _ : rawBoards) = lines contents
      input = parseInput rawInput
      boards = parseBoards rawBoards
      (winner, n) = simulate input boards ([], -1)
  print (n * unmarkedSum winner)

readInt :: String -> Int
readInt = read

parseInput :: String -> [Int]
parseInput s = map readInt $ splitOn "," s

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards l = parseBoard (take 5 l) : parseBoards (drop 6 l)

parseBoard :: [String] -> Board
parseBoard = map (map (\w -> (readInt w, False)) . words)

simulate :: [Int] -> [Board] -> (Board, Int) -> (Board, Int)
simulate [] _ t = t
simulate (x : xs) bs t@(b, n) =
  if null idx
    then simulate xs bs' t
    else simulate xs bs'' (head winners, x)
  where
    bs' = map (mark x) bs
    idx = findIndices checkBoard bs'
    winners = map (bs' !!) idx
    bs'' = bs' \\ winners

checkBoard :: Board -> Bool
checkBoard b = checkRows b || checkCols b

checkRows :: Board -> Bool
checkRows = any (all snd)

checkCols :: Board -> Bool
checkCols = checkRows . transpose

mark :: Int -> Board -> Board
mark _ [] = []
mark n (h : t) = go n h : mark n t
  where
    go :: Int -> [(Int, Bool)] -> [(Int, Bool)]
    go _ [] = []
    go n (t@(m, b) : l) = if n == m then (n, True) : go n l else t : go n l

unmarkedSum :: Board -> Int
unmarkedSum = foldr ((+) . sum . map (\(x, b) -> if b then 0 else x)) 0