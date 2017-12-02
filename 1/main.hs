import Data.Char;

digits :: String -> [Integer]
digits string = [ read [c] | c <- string, c `elem` ['0'..'9']]

sumSequential :: [Integer] -> Integer
sumSequential xs = run (length xs) (length xs `quot` 2)
  where run samples step = sum $ take samples $ zipWith foo xss (drop step xss)
        foo a b | a == b = a | otherwise = 0
        xss = cycle xs

main :: IO ()
main = do
  contents <- getContents
  putStr $ show $ sumSequential $ digits contents

