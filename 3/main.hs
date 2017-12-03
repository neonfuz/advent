
-- Direction code
data Direction = North | East | South | West deriving (Enum, Show, Eq)

turnRight :: Direction -> Direction
turnRight West = North
turnRight d = succ d

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft d = pred d
-- End direction code


type Pos = (Integer, Integer)

myAdd (x1, y1) (x2, y2) = (x1+x2, y1+y2)
distanceFromCenter (x, y) = abs x + abs y

move :: Direction -> Integer -> (Integer,Integer) -> Pos
move North steps (x, y) = (x, y + steps)
move South steps (x, y) = (x, y - steps)
move East  steps (x, y) = (x + steps, y)
move West  steps (x, y) = (x - steps, y)

iter :: Direction -> Integer -> Integer -> Pos -> Pos
iter dir step count pos@(x,y) =
  if count < step
  then
    move dir count pos
  else
    iter (turnLeft dir) (step + stepinc) (count - step) (move dir step pos)
  where stepinc = if ((dir == North) || (dir == South)) then 1 else 0

getPosition :: Integer -> Pos
getPosition addr = iter (South) 0 (addr-1) (0,0)

isNeighbor :: Pos -> Pos -> Bool
isNeighbor (x1,y1) (x2, y2) = (abs (x1-x2) < 2) && (abs (y1-y2) < 2)

getValue 1 = 1
getValue addr = sum neighborValues
  where pos = getPosition addr
        neighbors = filter (isNeighbor pos . getPosition) [1..(addr-1)]
        neighborValues = map getValue neighbors

values = map getValue [1..]

part1 = do
  contents <- getContents
  putStr $ show $ distanceFromCenter $ getPosition (read contents :: Integer)

part2 = do
  contents <- getContents
  putStr $ show $ head $ dropWhile (<(read contents :: Integer)) values

--input = 265149
main = part2
