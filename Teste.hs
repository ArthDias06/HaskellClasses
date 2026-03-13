--Este foi um exemplo básico pedido para o prfoessor para
--testermaos o básico de Haskell
main :: IO ()
main = do
    x <- readLn :: IO Int
    print (x+1)