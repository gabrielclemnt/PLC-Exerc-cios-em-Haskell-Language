import Data.Char (toLower)
import Data.List (sort, (\\))

uncommon :: String -> String -> [String]
uncommon sentence1 sentence2 =
    let word1 = words (map toLower sentence1)
        word2 = words (map toLower sentence2)
        uncommonWords = sort $ filter (\w -> length (filter (== w) (word1 ++ word2)) == 1) (word1 ++ word2)
    in uncommonWords

main :: IO ()
main = do
    sentence1 <- getLine
    sentence2 <- getLine
    let result = uncommon sentence1 sentence2
    print result
