{- calculadoramque recebe uma lista de tuplas com comandos e valores.
A função deve começar pelo valor 0. Por exemplo, caso a sequência de comandos seja
[("Multiplica", 2), ("Soma", 5), ("Subtrai", 3)]
a função deve pegar 0 e efetuar as seguintes operações: (((0 * 2) + 5) - 3).
Esses comandos podem ser "Multiplica", "Soma", "Subtrai" ou "Divide". Para o caso de uma divisão por 0, a função deve retornar o valor -666 independente de quanto tenha calculado até essa divisão.-}

-- a ideia é que os calculos sejam feitos de forma acumulada

type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa lista = executa' 0 lista -- chama uma função auxiliar chamada executa' com um acumulador inicial de 0 e a lista de comandos e valores.
  where
    executa' acumulado [] = acumulado
    executa' acumulado (("Multiplica", valor): xs) = executa' (acumulado * valor) xs
    executa' acumulado (("Soma", valor): xs) = executa' (acumulado + valor) xs
    executa' acumulado (("Subtrai", valor): xs) = executa' (acumulado - valor) xs
    executa' acumulado (("Divide", valor): xs) = 
     if valor == 0 
        then -666 
        else executa' (acumulado `div` valor) xs
    executa' acumulado (_:xs) = executa' acumulado xs


main = do
 a <- getLine
 let result = executa (read a :: [(Comando, Valor)])
 print result
