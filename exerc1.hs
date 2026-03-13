--Exercício pedido pela monitora para receber 3 inteiros positivos(não foi feita verificação de sinal)
--Verificar se eles podem formar um triângulo e caso o faça é pedida
--o cálculo da área dele. Foi utilizado >= na condição do if à demanda do exercício

main :: IO ()
main = do
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    z <- readLn :: IO Int
    if x+y >= z && y+z >= x && x+z >= y
        then print $ areaTriangulo x y z
        else putStrLn "-"

areaTriangulo :: Int -> Int -> Int -> Double --3 inputs e 1 output
areaTriangulo x y z = sqrt (p * (p - a) * (p - b) * (p - c)) --sqrt apenas aceita double
  where
    a = fromIntegral x --Tranforma em double
    b = fromIntegral y
    c = fromIntegral z
    p = (a + b + c) / 2 --Precisa de double