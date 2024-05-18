{- Considere unzip:: [(a,b)] -> ([a],[b]), definida como:

unzip:: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((a, b): xs) = (a:as, b:bs)
 where
   (as, bs) = unzip xs 
   
Exemplo:
unzip [(1,2), (3,4), (5,6)] ==> ([1,3,5],[2,4,6])

main = interact $ show . unzip' . (read :: String -> [(Int,Int)])-}

--define a função unzip', usando foldr.
unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([],[])
-- recebe uma tupla (a, b) e um par de listas (as, bs).
-- retorna um novo par de listas no qual o primeiro elemento é a adicionado a lista as e o segundo elemento é b adicionado a lista bs

main = interact $ show . unzip' . (read :: String -> [(Int,Int)])

