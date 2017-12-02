import Data.Char;

digits :: String -> [Integer]
digits string = [ read [c] | c <- string, c `elem` ['0'..'9']]

sumSequential :: [Integer] -> Integer
sumSequential nums = sum [ fst x | x <- pairs, uncurry (==) x ]
  where pairs = take (length nums) (zip infnums (tail infnums))
        infnums = cycle nums

main :: IO ()
main = do
  contents <- getContents
  putStr $ show $ sumSequential $ digits contents

