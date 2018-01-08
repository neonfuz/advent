
main = do
  contents <- getContents
  print $  main' contents

main' :: String -> Int
main' contents = iter 0 (insts,[])
  where insts = map (read :: String -> Int) $ lines contents

iter :: Int -> ([Int],[Int]) -> Int
iter step ([],_) = step
iter step ((i:front),back)
  | i > 0 = iter' (step+1) (drop (step-1) front, (reverse (take (step-1) front)) ++ [i+1] ++ back)
  | i < 0 = iter' (step+1) ((reverse (take (step-1) back)) ++ [i+1] ++ front, drop (step-1) back)
  | i == 0 = iter' (step+1) (((i+1):front),back)
