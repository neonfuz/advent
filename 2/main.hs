import Data.Char
import Data.List

main :: IO ()
main = do
  contents <- getContents
  putStr $ prog contents

prog input = show $ sum $ map checksum $ table
  where table = map ((map read) . words) $ lines input
        checksum row = maximum row - minimum row
