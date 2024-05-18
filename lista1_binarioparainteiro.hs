-- Binário para inteiro
--que, dada uma string representando um número binário, retorna o inteiro na base 10 dessa string.

--O bit menos significativo é o da direita.

btoi :: String -> Int
btoi [] = 0
btoi (x:xs) = (2 ^ (length xs)) * (read [x] :: Int) + btoi xs -- a função (length xs) calcula o numero de bits restantes, e 2^calcula elevado a esssa potencia
-- lenght determina o tamanho de uma lista

{-* (read [x] :: Int): Multiplica o valor posicional pelo valor do bit atual. A função read [x] :: Int converte o caractere x em um número inteiro.

+ btoi xs: Adiciona recursivamente o valor dos bits restantes, chamando a função btoi na parte restante da string.-}

main = do
    s <- getLine
    let result = btoi s
    print result

