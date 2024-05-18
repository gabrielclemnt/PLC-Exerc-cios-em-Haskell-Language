{-Gustavo é um estudante do ensino médio e recebeu a seguinte atividade do seu professor de matemática:

“Dado um número natural maior que um, decomponha-o em fatores primos e diga quantas vezes cada fator aparece.”

fatPrime :: Int -> [(Int, Int)]

O problema é que gustavo deixou para fazer a tarefa um dia antes da entrega e não conseguirá resolver a atividade a tempo. Sendo assim, ajude gustavo a não ficar com zero na atividade e crie um programa em haskell que seja capaz de resolver problema proposto.

A decomposição em fatores primos é um processo pelo qual escrevemos números compostos na forma de um produto em que todos os fatores são números primos.-}

--codando

--funcao que retorna uma lista com os fatores primos de um numero
fatoresPrimos :: Int -> [Int]
fatoresPrimos 1 = []
fatoresPrimos n = fator : fatoresPrimos (div n fator)
    where fator = menorDivisor n

--funcao que retorna o menor divisor de um numero
menorDivisor :: Int -> Int
menorDivisor n = head [x | x <- [2..n], mod n x == 0]

--funcao que retorna uma lista com os fatores primos e suas respectivas quantidades
fatPrime :: Int -> [(Int, Int)]
fatPrime n = [(x, length (filter (==x) (fatoresPrimos n))) | x <- (fatoresPrimos n), (length (filter (==x) (fatoresPrimos n))) > 0]

--funcao que retorna uma string com os fatores primos e suas respectivas quantidades
fatPrimeString :: Int -> String
fatPrimeString n = show (fatPrime n)

main = do
 a <- getLine
 let result = fatPrime (read a :: Int)
 print result



 -------------------------------------------------------------------------
 --correto
 fatoresPrimos :: Int -> [Int]
fatoresPrimos 1 = []
fatoresPrimos n = fator : fatoresPrimos (div n fator)
    where fator = menorDivisor n

menorDivisor :: Int -> Int
menorDivisor n = head [x | x <- [2..n], mod n x == 0]

fatPrime :: Int -> [(Int, Int)]
fatPrime n = [(x, count x (fatoresPrimos n)) | x <- nub (fatoresPrimos n)]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/=x) xs)

fatPrimeString :: Int -> String
fatPrimeString n = show (fatPrime n)

main :: IO ()
main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result
-----------------------------------------------------------------------------------