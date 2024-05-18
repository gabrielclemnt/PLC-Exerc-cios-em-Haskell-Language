--lista de múltiplos da lista

{-Crie a função Que recebe 1 lista de inteiros e um inteiro M, e retorna uma lista com a soma dos termos dos elementos de cada número na lista que são multiplos de M menores que o elemento da lista.:-}

{-Input [5, 10, 15] 3

A lista retornada é:
[3, 18, 45] 

-}


somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos xs m
  | m == 0 = replicate (length xs) 0  -- Se M for 0, a saída é uma lista de zeros
  | otherwise = go xs m []
    where
      go [] _ acc = reverse acc
      go (x:xs) m acc = go xs m (sumMultiplos x m : acc)

sumMultiplos :: Int -> Int -> Int
sumMultiplos x m = sum [y | y <- [1..x], y `mod` m == 0]

main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result

{--somarMultiplos :: [Int] -> Int -> [Int]: Declaração do tipo da função somarMultiplos, que recebe uma lista de inteiros xs e um inteiro m e retorna uma lista de inteiros.

somarMultiplos xs m: Início da definição da função somarMultiplos com parâmetros xs e m.

| m == 0 = replicate (length xs) 0: Uma guard clause (cláusula de guarda) que verifica se m é igual a zero. Se for verdadeiro, a função retorna uma lista com o mesmo comprimento de xs contendo apenas zeros, usando a função replicate.

| otherwise = go xs m []: Caso a condição da guard clause não seja atendida (otherwise), a função chama a função auxiliar go com os parâmetros xs, m e uma lista vazia [].

where: Início da cláusula where, onde são definidas funções auxiliares locais.

go [] _ acc = reverse acc: Definição da função go quando a lista xs é vazia. Retorna a lista acumulada acc revertida, pois os elementos são adicionados no início da lista para evitar a ineficiência de usar (++) repetidamente.

go (x:xs) m acc = go xs m (sumMultiplos x m : acc): Definição da função go quando a lista xs não é vazia. Chama recursivamente go com a cauda da lista xs, o mesmo valor m e a soma dos múltiplos de m até x adicionada à lista acc.

sumMultiplos :: Int -> Int -> Int: Declaração do tipo da função sumMultiplos, que recebe dois inteiros, x e m, e retorna um inteiro.

sumMultiplos x m = sum [y | y <- [1..x], y mod m == 0]: Implementação da função sumMultiplos, que calcula a soma dos múltiplos de m até x usando uma list comprehension.

main = do: Início da definição da função principal main.

lista <- getLine: Solicita a entrada do usuário para a lista de números como uma string e a armazena na variável lista.

let readList = read lista :: [Int]: Converte a string lista em uma lista de inteiros usando read e atribui o resultado a readList.

num <- getLine: Solicita a entrada do usuário para o número m como uma string e a armazena na variável num.

let readNum = read num :: Int: Converte a string num em um inteiro usando read e atribui o resultado a readNum.

let result = somarMultiplos readList readNum: Chama a função somarMultiplos com readList e readNum e atribui o resultado a result.

print result: Imprime o resultado na tela.




















}