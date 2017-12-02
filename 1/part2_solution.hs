import Data.Char;

digits :: String -> [Integer]
digits string = [ read [c] | c <- string, c `elem` ['0'..'9']]

sumSequential :: [Integer] -> Integer
sumSequential nums = sum [ fst x | x <- pairs, uncurry (==) x ]
  where pairs = take numLen (zip infnums (drop (quot numLen 2) infnums))
        infnums = cycle nums
        numLen = length nums

main :: IO ()
main = do
  contents <- getContents
  putStr $ show $ sumSequential $ digits contents

