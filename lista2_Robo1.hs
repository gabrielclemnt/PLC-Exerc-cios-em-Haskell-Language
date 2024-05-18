data Command = Forward Int | Backward Int | TurnLeft | TurnRight
               deriving (Eq, Show, Read)

data Direction = North | East | South | West
               deriving (Eq, Show)

type Position = (Int, Int)

destination :: Position -> [Command] -> Position
destination initialPos commands = fst $ foldl executeCommand (initialPos, North) commands
  where
    executeCommand :: (Position, Direction) -> Command -> (Position, Direction)
    executeCommand (pos, dir) (Forward n)  = (move pos dir n, dir)
    executeCommand (pos, dir) (Backward n) = (move pos dir (-n), dir)
    executeCommand (pos, dir) TurnLeft     = (pos, turn dir (-90))
    executeCommand (pos, dir) TurnRight    = (pos, turn dir 90)

    move :: Position -> Direction -> Int -> Position
    move (x, y) North n = (x, y + n)
    move (x, y) East n  = (x + n, y)
    move (x, y) South n = (x, y - n)
    move (x, y) West n  = (x - n, y)

    turn :: Direction -> Int -> Direction
    turn dir deg = case (dir, deg `mod` 360) of
      (North, 90)  -> East
      (North, 270) -> West
      (East, 90)   -> South
      (East, 270)  -> North
      (South, 90)  -> West
      (South, 270) -> East
      (West, 90)   -> North
      (West, 270)  -> South
      _            -> dir

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  let result = destination (read a) (read b)
  print result
