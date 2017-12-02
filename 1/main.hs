import Data.Char;

digits :: String -> [Integer]
digits string = [ read [c] | c <- string, c `elem` ['0'..'9']]

run :: Int -> Int -> [Integer] -> Integer
run samples step xs = sum $ [ a | (a,b) <- take samples pairs, a == b ]
  where pairs = zip tape (drop step tape)
        tape = cycle xs

part1solution xs = run (length xs) 1 xs
part2solution xs = run (length xs) (length xs `quot` 2) xs

main :: IO ()
main = do
  contents <- getContents
  putStr $ show $ part2solution $ digits contents

