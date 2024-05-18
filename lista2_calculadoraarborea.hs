{-
Calculadora Arborea

Dados os seguintes tipos algébricos:

data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)


Escreva a função

evalTree :: IntTree -> Int que calcula o valor resultante das operações na árvore dada.

As operações podem ser:
SUM = Soma
SUB = Subtração
MUL = Multiplicação

Utilize a seguinte função main para chamar evalTree:

main = do
    s <- getLine
    let result = evalTree (read s)
    print result

Input
(Node SUM (Node MUL (Nilt 5) (Nilt 3)) (Node SUB (Nilt 10) (Nilt 5)))

Output esperado
20   -}

data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree    -- Node recebe um valor Ops e dois valores IntTree como argumento
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node SUM a b) = evalTree a + evalTree b --avalia as subárvores esquerda e direita e retorne a soma delas
evalTree (Node SUB a b) = evalTree a - evalTree b -- avalia as subárvores esquerda e direita e retorne a diferença delas
evalTree (Node MUL a b) = evalTree a * evalTree b --avalie as subárvores esquerda e direita e retorne o produto delas

main = do
    s <- getLine
    let result = evalTree (read s)
    print result

