{-Robô 2
Considere os seguintes tipos:

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)
Faça a função

faces :: Direction -> [Command] -> Direction
que recebe a direção inicial do robô e uma lista de comandos e retorna para qual direção o robô estará voltado depois de executar essa lista de comandos.
O robô é controlado por 4 comandos:
Left, para girar sua direção em 90 graus no sentido anti-horário.
Right, para girar sua direção em 90 graus no sentido horário.
Forward seguido de um número N, que indica um avanço de N metros.
Backward seguido de um número N, que indica um retrocesso de N metros.


Utilize a seguinte função main para chamar faces:

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result 
       
       
exemplo:
Input
North
[Forward 2, TurnLeft, TurnLeft, Forward 1]

Output
South-}

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces dir [] = dir  -- se a lista de comandos estiver vazia a direção vai permanecer a mesma
faces dir (cmd:cmds) = 
    case cmd of
        Forward _   -> faces (moveForward dir) cmds
        Backward _  -> faces (moveBackward dir) cmds
        TurnLeft    -> faces (turnLeft dir) cmds
        TurnRight   -> faces (turnRight dir) cmds

-- manipulandoa direção
moveForward :: Direction -> Direction
moveForward North = South
moveForward South = North
moveForward West  = East
moveForward East  = West

moveBackward :: Direction -> Direction
moveBackward North = South
moveBackward South = North
moveBackward West  = East
moveBackward East  = West

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft South = East
turnLeft West  = South
turnLeft East  = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight South = West
turnRight West  = North
turnRight East  = South

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    let result = faces (read a) (read b)
    print result
