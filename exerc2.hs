import Text.Printf (printf)
--Esse exercício se baseou em ler dois números inteiros e depois
--Verificar quantos números, defeituosos, perfeitos e abundantes
--Existem entre eles.

--1. Número Perfeito: Um número inteiro positivo é chamado de número perfeito se a soma de seus divisores próprios (todos os divisores, excluindo o próprio número) for igual ao número. Por exemplo:
--6 é um número perfeito porque seus divisores próprios são 1, 2 e 3, e 1 + 2 + 3 = 6.

--2. Número Abundante: Um número inteiro positivo é chamado de número abundante se a soma de seus divisores próprios for maior que o número em si. Por exemplo:
--12 é um número abundante porque seus divisores próprios são 1, 2, 3, 4 e 6, e 1 + 2 + 3 + 4 + 6 = 16, que é maior que 12.

--3. Número Defeituoso: Um número inteiro positivo é chamado de número defeituoso se a soma de seus divisores próprios for menor que o número em si. Por exemplo:
--8 é um número defeituoso porque seus divisores próprios são 1, 2 e 4, e 1 + 2 + 4 = 7, que é menor que 8.7

main :: IO ()
main = do
    num1 <- readLn :: IO Int
    num2 <- readLn :: IO Int
    let lista = [num1..num2] --Cria uma lisatd e num1 até num2
    let listaFinal = map procuraDivisores lista --Executa procuraDivisores em cada elemento de lista
    --Como o retorno é uma diferença, os resultados positivos são defeituosos, os nulos são perfeitos e os negativos são abundantes.
    print $ length (filter (>0) listaFinal)
    print $ length (filter (==0) listaFinal)
    print $ length (filter (<0) listaFinal)

--Função que retorna a diferença entre o número e a soma de seus divisores
procuraDivisores x = x - sum listaDivisores
    where
        listaDivisores = [y | y <- [1..x-1], mod x y == 0]--O próprio número não pode estar entre os divisores