
answer :: Int 
answer = 42 

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int
square x = x * x

--tres paremetros inteiros e o resultado um bool
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n==m) && (m==p)

maxi :: Int -> Int -> Int
maxi n m | n>=m   = n  --se n for maior igual a m ent devolve 
         | otherwise = m -- se nao devolve m

vendas :: Int -> Int
vendas n = mod n 17

totalVendas :: Int -> Int  --haskel n usa laço 
totalVendas n | n == 0  = 0
              | n > 0  = vendas n + totalVendas (n - 1)   --pega n e soma totalde vendas n-1 isso dá o total de vendas
              | otherwise = 0

--defina uma função fatorial 
fatorial :: Int -> Int
fatorial n | n == 0 = 1
           | n > 0 = n * fatorial (n - 1)
           | otherwise = 0

--compare se quato numeros são iguais e depois crie outra função que retorna quantos parametros são iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal n m p q = (n==m) && (m==p) && (p==q)

equalCount :: Int -> Int -> Int -> Int -> Int
equalCount n m p q | (n==m) && (m==p) && (p==q) = 4
                   | (n==m) && (m==p) || (n==m) && (m==q) || (n==p) && (p==q) || (m==p) && (p==q) = 3
                   | (n==m) || (m==p) || (p==q) || (n==p) || (m==q) || (n==q) = 2
                   | otherwise = 0

sumSquares :: Int -> Int -> Int
sumSquares x y = sq x + sq y
 where sq z = z * z 

-- ou exclusivo
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

-- "peixe" ++ "\n" ++ "gato" + "\n" concatenação

--defina uma função addEspacos que produz uma string com uma quantidade n de espaços
addEspacos :: Int -> String
addEspacos n | n == 0 = ""
             | n > 0 = " " ++ addEspacos (n - 1)
             | otherwise = ""
