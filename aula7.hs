--funções de alta ordem map, fold, filter

-- monomórfica tem tipos de dados fixos
-- polimorfica, possui um tipo genérico, reuso de código
-- utiliza variáveis de tipo




applyBinOpen :: (t-> t-> t) -> t-> t-> t
applyBinOpen f x y = f x y   -- entrada (*) 20 10 resultado -> 200

-- função de concatenação ++

double :: [Int] -> [Int]
double [] = []
double (x:xs) = (2*x) : double xs

sqrList :: [Int] -> [Int]
sqrList [] = [] --
sqrList (y:ys) = (y*y) : sqrList ys


sqr :: Int -> Int
sqr x = x*x

--função de mapeamento
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = (f x) : mapInt f xs



--dada uma função veriicar se ela é crescente em um intervalo de 0 a n
isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n | f n >= f (n-1) = isCrescent f (n-1)
               | otherwise = False

-- uma funçao que eleva os itens de uma lista ao quadrado usando mapping
squareList :: [Int] -> [Int]
squareList [] = []
squareList (x:xs) = (x*x) : squareList xs

--uma função que retorna a  soma dos quadrados dos itens usando folding

sumSqrList :: [Int] -> Int
sumSqrList [] = 0
sumSqrList (x:xs) = (x*x) + sumSqrList xs

--outra maneira
{-sumSqr :: [Int] -> Int
sumSqr lista = foldr = foldr fun_aux 0 lista
    where fun_aux x acc = x*x + acc -}

-- uma função que mantenha na lista todos os numeros maiores que zero
list :: [Int] -> [Int]
list [] = []
list (x:xs) | x > 0 = x : list xs
            | otherwise = list xs


-- exemplos de classes haskell
{-Eq -> todos os tipos que eu consigo comparar os valores
Show -> todos os tipos os quais os valores eu consigo tranformar em uma string
Ord -> A classe Ord em Haskell é uma classe de tipos que representa tipos cujos valores podem ser ordenados. -}


