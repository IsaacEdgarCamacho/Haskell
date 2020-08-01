
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
                | n == 2     = es_aPseudoprimo n 2
                | otherwise  = es_aPseudoprimo n a   &&   es_aPseudoprimo n (n-1)



esCarmichael :: Integer -> Bool
esCarmichael n = (esCompuesto n) && es_aPseudoprimoParaTodos n (n-1)


