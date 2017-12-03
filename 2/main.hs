import Data.Char
import Data.List

main :: IO ()
main = do
  contents <- getContents
  putStr $ prog contents

checksum :: [Integer] -> Integer
checksum row = head [ a `quot` b | a <- row, b <- row, a /= b, a `mod` b == 0 ]

prog :: String -> String
prog input = show $ sum $ map checksum $ table
  where table = map ((map read) . words) $ lines input
