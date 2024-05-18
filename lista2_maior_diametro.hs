{-diâmetro de uma arvore

Dado o seguinte tipo algébrico:

data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)
escreva a função:

maiorDiametro :: Ord t => Tree t -> Int
que calcula o maior diâmetro de uma arvore. o diâmetro de uma árvore e maior distância entre entre dois nós.

Utilize a seguinte função main para chamar maiorDiametro:

-}

data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro Nilt = 0
maiorDiametro (Node _ esq dir) = max (1 + altura esq + altura dir) (max (maiorDiametro esq) (maiorDiametro dir))
  where 
    diametro Nilt = 0
    diametro (Node _ esq dir) = 1 + altura esq + altura dir
    
    altura Nilt = 0
    altura (Node _ esq dir) = 1 + max (altura esq) (altura dir)

main = do
    s <- getLine
    let result = maiorDiametro (read s :: Tree Int)
    print result
