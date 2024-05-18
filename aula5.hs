--funções de alta ordem : mapeamento, redução, filtragem

applyBinOper :: (t -> t -> t) -> t -> t -> t
applyBinOper f a b = f a b    -- f recebe  um oper e e é aplicado a (a e b)

-- applyBinOper (+) 3 4


-- função mapeapemnto , tranformar cada elemento de uma lista de t para u

map' :: (t -> u) -> [t] -> [u]
map' f [] = []
map' f (a:as) = f a : map' f as

seconds :: [(t, u)] -> [u]
seconds xs = map snd xs

-- redução 

maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = max (f n) (maxFun f (n-1))

-- foldr  ->  frequentemente usada para reduzir uma lista para um único valor, aplicando uma função acumuladora a cada elemento da lista.

-- dada uma função, verificar se ela é crescente em um intervalo de 0 a n
isCrescendo :: (Int -> Int) -> Bool
isCrescendo f = foldr (&&) True [f x <= f (x+1) | x <- [0..n-2]]
    where n = 1000
