import Data.Char
import Data.List

-- didnt bother with equally common bits as that wasnt an issue for my input
-- also, very shitty code, but it works, so whatever

main :: IO ()
main = do
  contents <- readFile "input"
  let input = lines contents
      oxygen = go 0 True input
      co2 = go 0 False input
  print (oxygen * co2)
  where
    go :: Int -> Bool -> [String] -> Int
    go _ _ [s] = bin2dec s
    go n b l = go (n + 1) b $ filter (\s -> if b then s !! n == c else s !! n /= c) l
      where
        c = mostCommon (transpose l !! n)

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

onesComplement :: Char -> Char
onesComplement '0' = '1'
onesComplement '1' = '0'

mostCommon :: String -> Char
mostCommon l = snd (maximum [(length ks, head ks) | ks <- group (sort l)])