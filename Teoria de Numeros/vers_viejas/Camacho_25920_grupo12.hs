-- Daniel: Comentario 1: No esta permitido hacer el tp con conocimientos
-- que no fueron explicados en la materia. Me refiero a 
-- esto que haces aca abajo, que es usar lo que se llama "listas por comprension".
-- En realidad, el tp se dio antes de las clases de listas, y esta pensado para hacer
-- por completo sin usar listas, de ningun tipo. 


divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n] , mod n x == 0]


esPrimo :: Integer -> Bool
esPrimo x = divisores x == [1,x]


--Un número natural n es compuesto si n > 1 y n no es primo 
--(por lo tanto el 1 no es ni primo ni compuesto).
esCompuesto :: Integer -> Bool
esCompuesto x = x > 1 && (esPrimo x) == False

--funcion que dadas dos listas me da la interseccion de ambas
interseccion :: [Integer] -> [Integer]-> [Integer]
interseccion n m = [x | x <- n , elem x m]


--Si a y n son números naturales, a y n son coprimos si el único 
--divisor positivo que tienen en común es el 1.
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos x y = (interseccion (divisores x) (divisores y)) == [1]


-- Daniel: Comentario 2: Esta funcion "potencia" no parece usarse
-- en ninguna otra parte del tp. Fijate de tratar de "limpiar el codigo"
-- antes de la entrega, para no entregar cosas que no hacen falta. 


potencia :: Integer -> Integer -> Integer
potencia base exponente  | exponente == 0 = 1
                         | base == 0     = 0
                         | otherwise = base * potencia base (exponente - 1)

-- Daniel: Comentario 3: Ojo que para que x sea 2-pseudoprimo tiene que ser
-- un numero compuesto. Tu funcion no parece estar chequeando esto. 


es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo x   = mod ( 2^(x-1)-1)  x  == 0

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo x   = mod ( 3^(x-1)-1)  x  == 0


-- Daniel: Comentario 4: Lo del rango grande esta mal, no hay ninguna garantia
-- de que el rango grande sea lo suficientemente grande. Tenes que buscar la forma 
-- de hacerlo usando recursion, sin apelar a que el numero este dentro de algun 
-- rango en particular. 

{- Esta funcion devuelve el k-esimo pseudoprimo pero no sabia como hacer un ciclo
    hasta hallar el kaesimo , entonces puse que un rango grande-}
eskPseudoprimo :: Integer -> [Integer]
eskPseudoprimo k = [x | x <- [3..(k*30)] , es2Pseudoprimo x  && es3Pseudoprimo x] 

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo a = eskPseudoprimo (a) !! fromIntegral a

{-Más en general, dado un número natural a, los a-pseudoprimos son los números naturales
compuestos n para los cuales n divide a a n−1 − 1.-}
aPseudoprimo :: Integer -> [Integer]
aPseudoprimo a = [x | x <- [1..(a*1000)] , (esCompuesto x ) && mod ( a^(x-1)-1)  x  == 0] 

esAPseudoprimo :: Integer -> Bool
esAPseudoprimo a = a `elem` aPseudoprimo a 


{-Finalmente, los números de Carmichael son los números naturales compuestos n que son a-
pseudoprimos para todo número natural a entre 1 y n − 1 que sea coprimo con n.

no llegue a implementarla

esCarmichael :: Integer -> Bool
esCarmichael n =  [x | x <- [1..(n-1)] , sonCoprimos(n x) &&  (esCompuesto n ) &&  (mod ( 2^(x-1)-1)  x  == 0)] 

-}


