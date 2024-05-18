
-- dividir a string em uma lista de strings usando espaços como delimitadores
dividirString :: String -> [String]
dividirString s = words [if c == ';' then ' ' else c | c <- s]

--converter uma lista de strings em uma lista de valores Double, pulando elementos de acordo com um índice
juntarString :: [String] -> Int -> [Double]
juntarString [] _ = []
juntarString (a:as) i
  | mod i 4 /= 0 = juntarString as (i + 1)
  | otherwise = read a : juntarString as (i + 1)

--encontrar o mínimo e o máximo em uma lista de valores Double
getMaxMin :: [Double] -> (Double, Double)
getMaxMin x = (minimum x, maximum x)

-- encontrar o mínimo e o máximo dos valores na fatura
minMaxCartao :: String -> (Double, Double)
minMaxCartao a = getMaxMin (juntarString (dividirString a) 1)

main :: IO ()
main = do
    a <- getLine
    let result = minMaxCartao a
    print result

--------------------------------------------------------------------------------------------------
-- Importa a função 'sort' do módulo 'Data.List', mas não é utilizada neste código
import Data.List (sort)

-- Função para dividir a string em uma lista de strings usando espaços como delimitadores
dividirString :: String -> [String]
dividirString s = words [if c == ';' then ' ' else c | c <- s]

-- Função para converter uma lista de strings em uma lista de valores Double, pulando elementos de acordo com um índice
juntarString :: [String] -> Int -> [Double]
juntarString [] _ = []  -- Caso base: a lista está vazia, retorna uma lista vazia
juntarString (a:as) i
  | mod i 4 /= 0 = juntarString as (i + 1)  -- Se o índice não é um múltiplo de 4, continua recursivamente sem adicionar o valor atual
  | otherwise = read a : juntarString as (i + 1)  -- Caso contrário, converte a string para Double e adiciona à lista, continua recursivamente

-- Função para encontrar o mínimo e o máximo em uma lista de valores Double
getMaxMin :: [Double] -> (Double, Double)
getMaxMin x = (minimum x, maximum x)

-- Função principal para encontrar o mínimo e o máximo dos valores na fatura
minMaxCartao :: String -> (Double, Double)
minMaxCartao a = getMaxMin (juntarString (dividirString a) 1)

-- Função principal de entrada e saída
main :: IO ()
main = do
    a <- getLine  -- Lê uma linha da entrada padrão
    let result = minMaxCartao a  -- Chama a função principal com a entrada
    print result  -- Imprime o resultado

-- https://github.com/ldcss/PLC---IF686/blob/main/lista_haskell/lista_1/B.hs