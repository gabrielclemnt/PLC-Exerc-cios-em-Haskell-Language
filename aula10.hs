--Laziness

f :: Int -> Int -> Int 
f x y = x + y

{-f(9-3) (f 3 5) = (9-3) + (f 3 5)
                = (9-3) + (3 + 5)
                = 6 + 8
                = 14 
-}

-- Lazy evaluation-> avaliação de uma expressão se dá apenas quando seu valor é necessário  
primos :: [Int]
primos = sieve [2..]

sieve :: [Int] -> [Int]  -- vai filtrar de forma que deixe apenas os primos entra take 10 primos
sieve (primo:lista) = primo : sieve [valor | valor <- lista, valor `mod` primo /= 0]   



--QUESTÃO DE PROVA
{-Run lenght Enconding é um processo para comprimir caracteres quando existe uma sequencia longa de caracteres repetidos. O algoritmo funciona, por exemplo trocando zeros
que se repetem em uma sequencia (lista) de numeros por zero seguido da quantidade de repetições

exemplo: 5 4 6 0 0 0 9 8 45 23
saida:   5 4 6 0 3 9 8 45 23

escreva uma função que comprime uma lista dessa forma e outra que descoprime-}

rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 (0: nums) = gerarZeros (head nums) ++ rldecode0 (tail nums)
rldecode0 (x:nums) = x : rldecode0 nums

gerarZeros :: Int -> [Int]
gerarZeros 0 = []
geraZeros n = 0 : gerarZeros (n-1)

rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 (0: nums) = 0 : (contaZeros nums +1) : rlencode0 (dropZeros nums)
elencode0  (x:nums) = x : rlencode0 nums

contaZeros :: [Int] -> Int
contaZeros [] = 0
contaZeros (0:nums) = 1 + contaZeros nums

dropZeros :: [Int] -> [Int]
dropZeros [] = []
dropZeros (0:nums) = dropZeros nums

isNumber :: Char -> Bool
isNumber char = char >= '0' && char <= '9'