-- Aplicação Parcial de funções e curryng semana 5
-- Tipos Algébricos semana 6

-- defina as seguintes funções 

data Expr = Lit Int       |
            Add Expr Expr |
            Sub Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add subexp1 subexp2) = "(" ++ showExpr subexp1 ++ "+" ++ showExpr subexp2 ++ ")"
showExpr (Sub subexp1 subexp2) = "(" ++ showExpr subexp1 ++ "-" ++ showExpr subexp2 ++ ")"

--------------------------------------------

teste3:: List Int
teste3 = Cons 1 (Cons 2 (Cons 3 Nil)) -- 1:2:3:[]

toList :: List t -> [t]
toList Nil = []
toList (Cons x xs) = x : toList xs

----------------------------------------------

data Tree1 t = Leaf | Node t (Tree1 t) (Tree1 t)
  deriving Show

data Tree2 t = Leaf2 t | Node2 (Tree2 t) (Tree2 t)
  deriving Show

test4, test5 :: Tree1 String
test4 = Node "a" (Node "b" Leaf Leaf) (Node "c" Leaf Leaf)
test5 = Node "aei" Leaf test4

depth :: Tree1 t -> Int
depth Leaf = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)


--------------------------------------------------
data List t = Nil | Cons t (List t)
  deriving Show

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

--------------------------------------------------------

data Tree t = Leaf | Node t (Tree t) (Tree t)
  deriving Show

collapse :: Tree t -> [t]
collapse Leaf = []
collapse (Node valor t1 t2) = valor : collapse t1 ++ collapse t2
---------------------------------------------------------

data Tree t = Leaf | Node t (Tree t) (Tree t)
  deriving Show

mapTree :: (t->u) -> Tree t -> Tree u
mapTree f Leaf = Leaf
mapTree f (Node valor t1 t2) = Node (f valor) (mapTree f t1) (mapTree f t2)
-------------------------------------------------------