
divisoresHasta :: Integer->Integer->Integer
divisoresHasta x y  | y == 1        = 1
                    | mod x y == 0  = 1 + divisoresHasta x (y-1)
                    | otherwise     = divisoresHasta x (y-1)


esPrimo :: Integer -> Bool
esPrimo x = divisoresHasta x x == 2


mcd :: Integer->Integer->Integer
mcd x y     | abs y > abs x     = mcd y x
            | y == 0            = abs x
            | otherwise         = mcd y (mod x y)


coprimos::Integer->Integer->Bool
coprimos x y = (mcd  x y) == 1

--Un número natural n es compuesto si n > 1 y n no es primo 
--(por lo tanto el 1 no es ni primo ni compuesto).

esCompuesto :: Integer -> Bool
esCompuesto x = (x > 1) && (esPrimo x) == False


es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo x   = (esCompuesto x) && mod ( 2^(x-1)-1)  x  == 0

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo x   = (esCompuesto x) && mod ( 3^(x-1)-1)  x  == 0

es4Pseudoprimo :: Integer -> Bool
es4Pseudoprimo  x   = (esCompuesto x) && mod ( 4^(x-1)-1)  x  == 0

-- cantidad3Pseudoprimos :: Integer -> Integer, que dado un número natural m calcula
-- la cantidad de 3-pseudoprimos que hay entre 1 y m inclusive.

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m 
    |   m == 0                 = 0
    |   es3Pseudoprimo m       = 1 + cantidad3Pseudoprimos (m-1)
    |   otherwise              = cantidad3Pseudoprimos (m-1)


-- kesimo2y3Pseudoprimo :: Integer -> Integer, que dado un número natural k calcula el
-- k-ésimo número que es simuláneamente 2-pseudoprimo y 3-pseudoprimo.
k2y3Pseudo :: Integer-> Integer-> Integer -> Integer
k2y3Pseudo x y z
    | x == 0                                    = z
    | es3Pseudoprimo y && es2Pseudoprimo y      = k2y3Pseudo (x-1) (y+1) (y)
    | otherwise                                 = k2y3Pseudo x (y+1) y


kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = k2y3Pseudo k 1 0 
  

es_aPseudoprimo :: Integer -> Integer -> Bool
es_aPseudoprimo n a   = (esCompuesto n) && mod ( a^(n-1)-1)  n  == 0


-- esCarmichael :: Integer -> Bool, que dado un número natural decide si es un número
-- de Carmichael (esta última función es opcional).
es_aPseudoprimoParaTodos::Integer->Integer->Bool
es_aPseudoprimoParaTodos n a
                | a == 1     = es_aPseudoprimo n 1
                | (coprimos n a) =   (es_aPseudoprimo n a)   &&   (es_aPseudoprimo n (a-1))
                | otherwise     = es_aPseudoprimo n (a-1)



-- esCarmichael :: Integer -> Bool
-- esCarmichael n =  length(   listaDecoPrimos n ) == length (listaDeCarmichael n)

carmichael :: Integer -> Integer -> Bool
carmichael n a
        |  a == 1                              =  True
        |  not (coprimos n a)                  =  carmichael n (a-1)
        |  otherwise                           =  ( mod ( a^(n-1)-1)  n  == 0 ) && carmichael n (a-1)




esCarmichael :: Integer->Bool
esCarmichael n 
        |  not(esCompuesto n)                    =  False
        |  otherwise                            =  carmichael n (n-1)
---------------------------------------------------------------------------------------
--PARTE OPCIONAL DE PRUEBA PARA CHEQUEAR LAS FUNCIONES
---------------------------------------------------------------------------------------
-- TEST DE LOS PRIMOS
-- HACER UNA FUCNION QUE DADO UN n NATURAL ME DE UNA LISTA CON TODOS LOS PRIMOS DESDE 2 hasta n
listaDecoPrimos :: Integer -> [Integer]
listaDecoPrimos n = [x | x <- [1..n] , coprimos  n x]


listaDePrimos :: Integer -> [Integer]
listaDePrimos n = [x | x <- [1..n] , esPrimo x]

listaDe2Primos :: Integer -> [Integer]
listaDe2Primos n = [x | x <- [1..n] , es2Pseudoprimo x]

listaDe3Primos :: Integer -> [Integer]
listaDe3Primos n = [x | x <- [1..n] , es3Pseudoprimo x]

listaDe4Primos :: Integer -> [Integer]
listaDe4Primos n = [x | x <- [1..n] , es4Pseudoprimo x]

listaDeAPrimos:: Integer-> Integer->[Integer]
listaDeAPrimos n a =  [x | x <- [1..n] ,     es_aPseudoprimo x a]


listaDeCarmichael:: Integer-> [Integer]
listaDeCarmichael n =  [x | x <- (listaDecoPrimos n) ,  esCompuesto n &&   mod ( x^(n-1)-1)  n  == 0]

todosCarmichael:: Integer-> [Integer]
todosCarmichael n =  [x | x <- [1..n] ,     esCarmichael x ]



-- es_aPseudoprimoParaTodos::Integer->Integer->Bool
-- es_aPseudoprimoParaTodos n a
--                 | n == 2     = es_aPseudoprimo n 2
--                 | otherwise  = es_aPseudoprimo n a   &&   es_aPseudoprimo n (n-1)

        