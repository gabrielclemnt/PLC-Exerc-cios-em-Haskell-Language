
-- Função para dividir a string em uma lista de strings usando espaços como delimitadores
dividirString :: String -> [String]
dividirString mes = words [if c == ';' then ' ' else c | c <- mes]

-- Função para juntar os valores correspondentes aos meses em uma lista de strings
juntarMesesString :: [String] -> Int -> [String]
juntarMesesString [] _ = []
juntarMesesString (a:as) i
  | i `mod` 4 /= 0 && i `mod` 2 == 0 = a : juntarMesesString as (i + 1)
  | otherwise = juntarMesesString as (i + 1)

-- Função para juntar os valores correspondentes aos gastos em uma lista de strings
juntarValoresString :: [String] -> Int -> [Double]
juntarValoresString [] _ = []
juntarValoresString (a:as) i
  | i `mod` 4 == 0 = read a : juntarValoresString as (i + 1)
  | otherwise = juntarValoresString as (i + 1)

-- Função para adicionar tuplas de mês e valor
adicionarTupla :: [String] -> [Double] -> [(String, Double)]
adicionarTupla [] _ = []
adicionarTupla a [] = []
adicionarTupla (a:as) (b:bs) = (a, b) : adicionarTupla as bs

-- Função para obter todas as tuplas de mês e valor a partir da string da fatura
obterTodasAsTuplas :: String -> [(String, Double)]
obterTodasAsTuplas mes = adicionarTupla (juntarMesesString (dividirString mes) 1) (juntarValoresString (dividirString mes) 1)

-- Função para obter os valores de gastos por mês a partir de uma lista de tuplas
valoresPorMes :: String -> [(String, Double)] -> [Double]
valoresPorMes [] as = []
valoresPorMes mes [] = []
valoresPorMes mes (a:as)
  | mes == fst a = snd a : valoresPorMes mes as
  | otherwise = valoresPorMes mes as

-- Função para obter a soma total dos valores
obterValores :: [Double] -> Double
obterValores (a:[]) = a
obterValores (a:as) = a + obterValores as

-- Função principal para calcular o total de gastos de um determinado mês
logMes :: String -> String -> Double
logMes mes ano = foldl (+) 0 (valoresPorMes mes (obterTodasAsTuplas ano))

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result