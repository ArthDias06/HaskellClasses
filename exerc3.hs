{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use foldr" -}

data Pessoa = Pessoa {
                nome :: String,
                tel :: String
                } deriving (Show, Eq)

--Show: Mostrar as informações
--Eq: Uso de equações entre as informações

loop listaPessoas = do
    input <- getLine
    --wrods: Separa as palavras de uma dada entrada
    let info = words input
    --Head: Pega o primeiro elemento de uma lista, nesse caso, a operação
    let operacao = head info
    case operacao of
        "adicionar" -> do
            --Adicionar um nome já existente é equivalente a atualizá-lo,
            --então eu removo quaisquer nomes existentes e depois adiciono
            let novaLista1 = remover info listaPessoas
            let novaLista2 = adicionar info novaLista1
            loop novaLista2
        "listar" -> do
            listar listaPessoas
            loop listaPessoas
        "buscar" -> do
            buscar info listaPessoas
            loop listaPessoas
        "remover" -> do
            let novaLista = remover info listaPessoas
            --Se as listas forem iguais após a remoção, não houve ninguém removido
            if novaLista == listaPessoas
                then putStrLn "Contato nao encontrado."
                else putStrLn "Contato removido."
            loop novaLista
        "sair" -> putStrLn "Encerrando."
        --Qualquer outro input de operação é desconsiderado
        _ -> do
            putStrLn "Opção inválida"
            loop listaPessoas


main :: IO()
--Chama o loop e inicializa a listaPessoas
main = loop []

-- : adicionana no início da lista
adicionar info lista = pessoa : lista
    where
        --Criando uma pessoa com nome e telefone passados, lista !! ínidce, retorna o item naquela posição da lista
        pessoa = Pessoa {nome = info !! 1, tel = info !! 2}

listar [] = return ()
-- >>: Execute depois da primeira instrução
--print nome x e tel x utiliza do Show para pegar as informações corretas
listar (x:xs) = putStrLn (nome x ++ " - " ++ tel x) >> listar xs


-- Retorna uma lista com todos as pessoas que correspondem à busca
buscar _ [] = putStrLn "Contato nao encontrado."
buscar info (x:xs)
    | nome x == info !! 1 = putStrLn (nome x ++ " - " ++ tel x)
    | otherwise = buscar info xs

--Retorna uma lista de pessoas filtrada sem as que foram removidas
remover info listaPessoas = filter (\x -> nome x/=info !! 1) listaPessoas