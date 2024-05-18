

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] _ = []
decEnigma (x:xs) par = subs x par : decEnigma xs par
  where
    subs c [] = c
    subs c ((a,b):ps)
      | c == a    = b
      | otherwise = subs c ps

main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b :: [(Char, Char)])
    print result


    decEnigma :: String -> [(Char, Char)] -> String
    -- Função para decifrar uma String usando um mapeamento de caracteres.
    -- Recebe uma String (mensagem a ser decifrada) e uma lista de tuplas (mapeamento de caracteres).
    -- Retorna uma String (mensagem decifrada).
    
    decEnigma [] _ = []
    -- Padrão de correspondência: se a lista de entrada for vazia, retorna uma lista vazia, indicando o fim da recursão.
    
    decEnigma (x:xs) par = subs x par : decEnigma xs par
    -- Padrão de correspondência para lista não vazia:
    -- Chama a função subs para obter o caractere decifrado correspondente a x e concatena com a chamada recursiva da função para o restante da lista e o mesmo mapeamento.
    
      where
        subs c [] = c
        -- Função local subs: Se a lista de mapeamento estiver vazia, retorna o próprio caractere.
    
        subs c ((a,b):ps)
          | c == a    = b
          -- Se c for igual ao primeiro elemento da tupla (a, b), retorna o segundo elemento b.
          | otherwise = subs c ps
          -- Caso contrário, continua procurando na lista de mapeamento ps.
    
    main = do
        a <- getLine
        -- Obtém uma linha de entrada do usuário para a variável a.
    
        b <- getLine
        -- Obtém outra linha de entrada do usuário para a variável b.
    
        let result = decEnigma a (read b :: [(Char, Char)])
        -- Chama a função decEnigma com os valores fornecidos pelo usuário, convertendo b de uma string para uma lista de tuplas.
    
        print result
        -- Imprime o resultado na tela.
    