import Data.Char
import Data.List

main :: IO ()
main = do
  contents <- readFile "input"
  let gamma = map (\l -> snd (maximum [(length ks, head ks) | ks <- group (sort l)])) $ transpose $ lines contents
  let epsilon = map onesComplement gamma
  print (bin2dec gamma * bin2dec epsilon)

bin2dec :: String -> Int
bin2dec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

onesComplement :: Char -> Char
onesComplement '0' = '1'
onesComplement '1' = '0'