import Data.Array (listArray)
--Faça um programa em Haskell, utilizando os conceitos de programação do paradigma funcional,
--que leia dois inteiros x e y, sendo que x é menor que y, e imprima o comprimento do maior intervalo
--entre dois números primos consecutivos maiores ou iguais a x e menores ou iguais a y.

--A lógica foi montar a lista de números primos no intervalo dado e subtrair
--cada elemento pelo seu sucessor e ver o maior resultado.
main :: IO ()
main = do
    num1 <- readLn :: IO Int
    num2 <- readLn :: IO Int
    let listaPrimos = primos num1 num2
    --zipWith: Executa a operação em cada elemento correspondente de cada lista,
    --em listas desiguais o resultado tem o tamanho da menor
    --drop n: retorna a lista com exceção dos n primeiros elementos.
    --Melhor que tail por não quebrar com listas vazias
    let listaFinal = zipWith (-) (drop 1 listaPrimos) listaPrimos
    print $ resultado listaFinal

--Função all verifica se a condição abrange todos os números da lista.
--i varia de 2 até x-1, assim se x for dividido por qualquer valor entre 1 e x
--Ele não é primo e não entra na lista.

--A expressão após all é uma expressão lambda, \ inicia a expressão
--i é o input e o que vem após -> é o output.
--Analogamente: a expressão equivale a para todo i pertencente a [2..x-1], x mod i != 0
primos a b = [x | x <- [a..b], x > 1 && all (\i -> mod x i /= 0) [2..x-1]]

--maximum: Retorna o maior valor de uma lista
--Quebra com listas vazias, então é feita verificação
resultado lista
    | null lista = 0
    |otherwise = maximum lista