import Data.Char (ord)

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

somaFibo :: Int -> Integer
somaFibo n = sum [x | x <- take n fibonacci, even x]

main :: IO ()
main = do
    input <- getLine
    let tamanhoString = length input
        resultado = "\"" ++ input ++ show (somaFibo tamanhoString) ++ "\""
    putStrLn resultado
