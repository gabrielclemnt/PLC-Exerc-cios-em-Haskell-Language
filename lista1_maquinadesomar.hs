--Dado uma lista de números inteiros, retorne uma lista de somas de intervalos desses números, tais que cada soma retornada é de números que estão separados por 0's, caso dois zeros lidos em sequência retorne a lista atualmente computada..
{-exemplo de input [0,1,2,3,4,0,0,2,3,2]
Output [10] -}

maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar lista = maquinaSomar' lista 0
    where
        maquinaSomar' [] _ = []
        maquinaSomar' (0:xs) _ = maquinaSomar' xs 0
        maquinaSomar' (x:xs) acumulador = maquinaSomar' xs (acumulador + x) ++ [acumulador + x]


main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])
---------------------------------------------------------------

       maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar lista = reverse $ maquinaSomar' lista 0 False []
  where
    maquinaSomar' [] acumulador _ resultado = if acumulador /= 0 then acumulador : resultado else resultado
    maquinaSomar' (0:0:_) acumulador _ resultado = acumulador : resultado
    maquinaSomar' (0:xs) acumulador True resultado = maquinaSomar' xs 0 True (acumulador : resultado)
    maquinaSomar' (0:xs) acumulador False resultado = maquinaSomar' xs 0 True resultado
    maquinaSomar' (x:xs) acumulador True resultado = maquinaSomar' xs (acumulador + x) True resultado
    maquinaSomar' (x:xs) acumulador False resultado = maquinaSomar' xs acumulador False resultado

main :: IO ()
main = do
   lista <- getLine
   print $ maquinaSomar (read lista :: [Int])




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