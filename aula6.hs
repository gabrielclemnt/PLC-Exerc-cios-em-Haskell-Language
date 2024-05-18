

-- POLIMORFISMO, CLASSSES E INSTANCIAS
-- Overloading

{-FUnções polimorficas 

(==) :: t-> t -> Bool

(<) :: t -> t-> Bool

show :: t-> String 
-}

rep 0 ch = []
rep n ch = ch : rep (n-1) ch

{- classes são usadass para permitir sobrecarga (overloading) de nomes
-}

--funções que usam iqualdade
allEqual :: Eq t => t -> t -> t -> Bool
allEqual n m p = (n == m) && (m == p)


--questão
{-faça uma função agrupar que recebe uma lista de listas de valores de um tipo t que podem ser comparados para saber se são iguais
 e devolve uma lista de pares (t, Int) onde o primeiro elemento é um valor do tipo t que existe em pelo menos uma das sub-listas da entrada
 e o segmento é o número de ocorrências desse valor nas sub listas -}


agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar [] = [] 
agrupar (a:as) = agruparAux a as ++ agrupar as
    where agruparAux x [] = [x]
          agruparAux x (y:ys) | x == y = agruparAux x ys
                              | otherwise = agruparAux x ys ++ [y]
 {- função que recebe uma lista com valores repetidos e retorna um dicionário com esses valores como chaves  --concat-}

-------------------------------------------------------------------------------------------------------------------
concatenar :: [[t]] -> [t]
concatenar [] = []
concatenar (lista:listas) = lista ++ concatenar listas

contar :: Eq t => t -> [t] -> Int
contar x [] = 0
contar x (a:as) | x == a = 1 + contar x as
                | otherwise = contar x as

agrupar :: Eq t => [[t]] -> [(t,Int)]
agrupar lista = contaElem (concatenar lista)
    where contaElem [] = []
          contaElem (a:as) = (a, contar a (concatenar lista)) : contaElem as
-----------------------------------------------------------------------------------------------------------------------
--posso usar tbm
contar' x lista = lenght [y | y <- lista, x == y]
