
data Direction = North | East | South | West deriving (Enum, Show)

turnRight :: Direction -> Direction
turnRight West = North
turnRight d = succ d

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft d = pred d

main = do
  contents <- getContents
  putStr contents

