-- Funções como valores

-- A função de composição ( f . g ) x = f ( g x)     na entrada (f . g) 3 vai ter como resultado 40 
f:: Int-> Int
f y = y * 10

g:: Int -> Int
g x = x+1

h :: Int -> Int
h = (f . g)

-- (.) :: (u -> v) -> (t-> u) -> (t -> u)

menorQue10 :: Int -> Bool 
menorQue10 x = x < 10

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f) .f  --iter 10 ((*)2) 3  multiplica 2*3 e depois vai multiplicando por 2 ate 10


sqr2 = \x -> x *x 


concat2 = \l1 l2 -> l1 ++ l2

fill = splitLines . splitWords

splitLines = map (\s -> [s])
splitWords = concat . map splitWords'


-- expressões que definem funções
-- \m -> 3+m
--addNum n = (\n -> n+m)

--exercicio: dada uma função f do tipo t -> u -> v, defina uma expressão da forma (\... -> ...) para uma função do tipo u -> t -> v que se comporta como f mas recebe seus argumentos na ordem inversa
-- inverte div 5 50 e no output 10
inverte :: (t -> u -> v) -> (u -> t -> v)
inverte f x y = f y x

-- (inverte div) 5 50

--(\f -> \x -> \y -> f y x) div 5 50




-- APLICAÇÃO PARCIAL DE FUNÇÕES

multiply :: Int -> Int -> Int
multiply x y = x * y

doubleList :: [Int] -> [Int]
doubleList = map (multiply 2)

whiteSpace = " "

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f) .f  --iter 10 ((*)2) 3  multiplica 2*3 e depois vai multiplicando por 2 ate 10


