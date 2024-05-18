--TUPLA ESTRUTURA DE DADOS

intP :: (Int, Int)
intP = (33,43)

addPair :: (Int, Int) -> Int
addPair (x,y) = x+y

-- usar tuplas para usar sinonimos de tipos


type Name = String
type Age = Int
type Phone = Int

type Person = (Name, Age, Phone)

name :: Person -> Name
name (n,a,p) = n

--bhaskara
--ax2 + bx + c = 0.0
-- x1 = (-b + sqrt(b^2 - 4*a*c)) / 2*a
-- x2 = (-b - sqrt(b^2 - 4*a*c)) / 2*a

oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b/(2*a)

twoRoots :: Float -> Float -> Float -> (Float, Float)
twoRoots a b c = (d-e, d+e)
    where d = -b/(2.0*a)
          e = sqrt(b*b - 4.0*a*c) / (2.0*a)

roots :: Float -> Float -> Float -> (Int, Float, Float)
roots a b c | b*b - 4*a*c < 0 = (0,0,0)
            | b*b - 4*a*c == 0 = (1, (oneRoot a b c), 0)
            | otherwise  = (2, raiz1, raiz2)  -- fst devolve o primeiro elemento de uma tupla e snd devolve o segundo elemento de uma tupla
              where (raiz1, raiz2) = twoRoots a b c


--compreensão de listas

-- [expressao que usa variavel | variavel <- lista, cond]

--[x*x | x <- [1..10]] -- eleva todos os numeros ao quadrado

--[x | x <- [1..100], x `mod` 7 == 0] --quero os numeros de uma lista tais que o resto da divisão por 7 é 0

--simulação de emprestimo de livros
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Gabriel","O senhor dos Aneis"), ("Laura", "Duna"), ("Leticia", "A culpa é das estrelas")]
--livros emprestados

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [ll | (pp,ll) <- bd, pp == p ] --essa pessoa que tomou o livro emprestado é a pessoa na qual to interessado

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [pp | (pp, ll) <- bd, ll == l]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l = length (emprestimos bd l) > 0      --lenght tamanho de emprestimos ou posso usar (emprestimos l) /= []

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length (livros bd p)    -- pega os livros emprestados e calcula o tamanho dessa lista de livros

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] pessoa livro = [(pessoa,livro)] --verificar se da para emprestar ou n
emprestar ((p,l):as) pessoa livro
    | p == pessoa && livro == l = ((p,l):as)
    | otherwise = (p,l) : emprestar as pessoa livro                                  

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] pessoa livro = []
devolver ((p,l):as) pessoa livro
    | p == pessoa && livro == l = as
    | otherwise = (p,l) : devolver as pessoa livro


-- Defina uma função que ordena uma lista de inteiros utilizando o algoritmo quick sort, usando mão de compressões de listas

quickSort :: [Int] -> [Int]
quickSort [] = [] --caso base
quickSort (a:as) = quickSort [x | x <- as, x < a] ++ [a] ++ quickSort [x | x <- as, x >= a]


--exercicio de list comprehension
--1) Defina uma função que recebe uma lista de inteiros e retorna uma lista com o dobro de cada elemento da lista recebida
dobro :: [Int] -> [Int]
dobro [] = []
dobro (a:as) = [2*a] ++ dobro as

-- filtragem: apenas os números de uma string
digits :: String -> String
digits [] = []
digits (x:xs) | x >= '0' && x <= '9' = x : digits xs

--defina se um elemento esta na lista
member :: [Int] -> Int -> Bool
member [] n = False
member (a:as) n | a == n = True
                | otherwise = member as n

--somar os elementos de duas listas
somaListas :: [Int] -> [Int] -> [Int]
somaListas [] [] = []
somaListas (a:as) (b:bs) = [a+b] ++ somaListas as bs -- ++ operador de concatenação


