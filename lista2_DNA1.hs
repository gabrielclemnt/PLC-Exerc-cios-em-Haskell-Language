data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

listaAjustada :: [String] -> [String] 
listaAjustada []        = []
listaAjustada listOfStr = [(fst (concaString [] listOfStr))] ++ (listaAjustada (removList (snd (concaString [] listOfStr)) listOfStr))

concaString :: String -> [String] -> (String, Int)
concaString str [] = (str, (length str))
concaString str (x:xs) | (length str) >= 8 = (str, (length str))
                   | otherwise = concaString (str ++ x) xs

removList :: Int -> [String] -> [String]
removList n [] = []
removList n list@(x:xs) | n == 0 = list
                             | otherwise = removList (n-1) xs

arvorList :: Tree Int -> [String]
arvorList Nilt = []
arvorList (Node x y z) | (mod x 5) == 0 = (arvorList y) ++ ["E"] ++ (arvorList z)
                        | (mod x 5) == 1 = (arvorList y) ++ ["M"] ++ (arvorList z)
                        | (mod x 5) == 2 = (arvorList y) ++ ["A"] ++ (arvorList z)
                        | (mod x 5) == 3 = (arvorList y) ++ ["C"] ++ (arvorList z)
                        | (mod x 5) == 4 = (arvorList y) ++ ["S"] ++ (arvorList z)

dna1 :: Tree Int -> [String]
dna1 tree = listaAjustada (arvorList tree)

main :: IO ()
main = do
  input <- getLine
  let result = dna1 (read input :: Tree Int)
  print result