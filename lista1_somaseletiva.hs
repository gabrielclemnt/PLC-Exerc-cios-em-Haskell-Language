-- Máquina de Somar
{-Dado uma lista de números inteiros, retorne uma lista da soma de intervalos desses números, tais que cada soma retornada é de números que estão entre 0's, caso dois zeros lidos em sequência retorne a lista com a soma atual-}

--exemplo1 de input [0,0,1,2,3,0,1]
--Output []

--exemplo2 de input [1,0,0,1]
--output [1]

--exemplo3 de input [0,1,2,3,4,0,0,2,3,2]
--output [10]

--exemplo4 de input [0,0,1]
--output []

--exemplo5 de input [0,2,3,4,0]
--output [9]

--exemplo6 de input [0,0,1,2,3,0,0,0,0,0,0,0,0,0,0,0,0,1]
--output []

maquinaSomar :: [Int] -> [Int]
maquinaSomar xs = go xs []
  where
    go [] acc = acc
    go (0:0:_) acc = acc  -- sequência 0,0 encontrada, encerra a computação
    go (x:xs) acc
      | x == 0 = go xs acc
      | otherwise =
        let (soma, rest) = span (/= 0) xs
        in go rest (acc ++ [x + sum soma])

main :: IO ()
main = do
  lista <- getLine
  print $ maquinaSomar (read lista :: [Int])


  