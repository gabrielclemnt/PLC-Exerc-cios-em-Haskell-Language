data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)


contains :: Ord t => t -> Tree t -> Bool
contains _ Nilt = False
contains x (Node value left right)
  | x < value  = contains x left
  | x > value  = contains x right
  | otherwise  = True 


isBST :: Ord t => Tree t -> Bool
isBST tree = isBST' tree Nothing Nothing
  where
    isBST' Nilt _ _ = True
    isBST' (Node value left right) minVal maxVal =
      let leftIsValid  = isBST' left minVal (Just value)
          rightIsValid = isBST' right (Just value) maxVal
      in case (minVal, maxVal) of
        (Just minV, Just maxV) -> minV < value && value < maxV && leftIsValid && rightIsValid
        (Nothing, Just maxV)    -> value < maxV && leftIsValid && rightIsValid
        (Just minV, Nothing)    -> minV < value && leftIsValid && rightIsValid
        (Nothing, Nothing)      -> leftIsValid && rightIsValid

main :: IO ()
main = do
  s <- getLine
  let result = isBST (read s :: Tree Int)
  print result
