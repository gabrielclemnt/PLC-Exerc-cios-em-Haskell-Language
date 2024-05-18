--Listas e Casamento de Padrões

-- [5] == 5:[]    também [1,2,3] == 1:2:3:[]

-- listas enumeradas [2..7] == [1,2,3,4,5,6,7]

-- head [1,2,3] tail [4,5,6]

-- [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]

--dobrar elementos de uma lista
double :: [Int] -> [Int]
double [] = []
double (x:xs) = 2*x : double xs

--ou então

dobrar :: Int -> Int 
dobrar x =  2 * x

lista :: [Int]
lista = [1,2,3,4,5,6,7,8,9,10]

listadobrada :: [Int]
listadobrada = map dobrar lista 


{-Defina uma função que, dado um valor inteiro s e
um número de semanas n, retorna quantas
semanas de 0 a n tiveram vendas iguais a s. Para
resolver esta questão, primeiro construa uma
definição simples para vendas.-}

vendas :: Int -> Int -> Int
vendas 0 = 30
vendas 1 = 50
vendas 2 = 80
vendas 3 = 60
vendas 4 = 50
vendas n = 0

vendasIguais :: Int -> Int -> Int
vendasIguais valor 0 | valor == vendas 0 = 1 --caso base
                     | otherwise = 0
vendasIguais valor n | valor == vendas n = 1 + vendasIguais valor (n-1)
                     | otherwise = vendasIguais valor (n-1) 



ehPrimo :: Int -> Bool
--ehPrimo 21 ==> False
--ehPrimo 17 ==> True
ehPrimo 1 = False
-- ehPrimo 2 = True
ehPrimo n = testaPrimo n (n`div`2)
 where testaPrimo x 1 = True
       testaPrimo x y | mod x y == 0 = False
                      | otherwise = testaPrimo x (y-1)


-- Casamento de Padrôes -> permite usar padrões no lugar de variaveis

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = max (vendas n) (maxVendas (n-1))

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)


--para negar valores
not :: Bool -> Bool
not True = False
not False = True