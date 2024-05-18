
vendas :: Int -> Int -> Int
vendas 0 = 30
vendas 1 = 50
vendas 2 = 80
vendas 3 = 60
vendas 4 = 50
vendas n = 0

addEspacos :: Int -> String
addEspacos n | n == 0 = ""
             | n > 0 = " " ++ addEspacos (n-1)
             | otherwise = ""
             
--  :l aula3.hs
-- /////////////////////////////////////////////////////////////////

paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = max (maxVendas (n-1)) (vendas n)


imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho 
                  ++ imprimeSemanas n
                  ++ imprimeTotal n
                  ++ imprimeMedia n
                  )

cabecalho :: String
cabecalho = "Semana" ++ paraDireita 5 "Vendas\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ paraDireita 5 (show (totalVendas n) ++ "\n")

imprimeMedia :: Int -> String
imprimeMedia n = "Media" ++ paraDireia 5 (show (mediasVendas n) ++ "\n")

mediaVendas :: Int -> Double
mediaVendas n = fromIntegral (totalVendas n) / fromIntegral (n+1) --fromIntegral convertei inteiro para float

imprimeSemanas :: Int -> String
imprimeSemanas 0 = paraDireita 3 (show 0) ++ paraDireita 8 (show (vendas 0)) ++ "\n"
imprimeSemanas n = imprimeSemanas (n-1) ++ 
                   paraDireita 3 (show n) ++ paraDireita 8 (show (vendas n)) ++ "\n"

