
import Data.List

numDuplicates line = foldl1 max $ map length $ group $ sort $ words line

main = do
  contents <- getContents
  print $ sum $ map valMap $ map numDuplicates $ lines contents
    where valMap x | x > 1 = 0 | otherwise = 1

