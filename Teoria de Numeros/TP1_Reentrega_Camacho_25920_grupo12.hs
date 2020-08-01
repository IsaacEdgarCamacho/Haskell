--             TP1 TALLER DE ALGEBRA
-------------------------------------------------------
-- (1ra parte): Números pseudoprimos y de Carmichael
-------------------------------------------------------
-- ALUMNO:     Isaac Camacho
-- Legajo:     259/20
-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------
--  FUNCIONES AUXILIARES
-------------------------------------------------------


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

esCompuesto :: Integer -> Bool
esCompuesto x = (x > 1) && (esPrimo x) == False


es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo x   = (esCompuesto x) && mod ( 3^(x-1)-1)  x  == 0

k2y3Pseudo :: Integer-> Integer-> Integer -> Integer
k2y3Pseudo x y z
    | x == 0                                    = z
    | es3Pseudoprimo y && es2Pseudoprimo y      = k2y3Pseudo (x-1) (y+1) (y)
    | otherwise                                 = k2y3Pseudo x (y+1) y


es_aPseudoprimo :: Integer -> Integer -> Bool
es_aPseudoprimo n a   = (esCompuesto n) && mod ( a^(n-1)-1)  n  == 0


carmichael :: Integer -> Integer -> Bool
carmichael n a
        |  a == 1                              =  True
        |  not (sonCoprimos n a)               =  carmichael n (a-1)
        |  otherwise                           =  ( mod ( a^(n-1)-1)  n  == 0 ) && carmichael n (a-1)

--------------------------------------------------------------
--      FUNCIONES DEL TP
--------------------------------------------------------------

sonCoprimos::Integer->Integer->Bool
sonCoprimos x y = (mcd x y) == 1

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo x   = (esCompuesto x) && mod ( 2^(x-1)-1)  x  == 0

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m 
    |   m == 0                 = 0
    |   es3Pseudoprimo m       = 1 + cantidad3Pseudoprimos (m-1)
    |   otherwise              = cantidad3Pseudoprimos (m-1)


kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = k2y3Pseudo k 1 0 

---------------------------------------------------------------------------------------
--PARTE OPCIONAL 
---------------------------------------------------------------------------------------
esCarmichael :: Integer->Bool
esCarmichael n 
        |  not(esCompuesto n)                   =  False
        |  otherwise                            =  carmichael n (n-1)
---------------------------------------------------------------------------------------




---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
-- TEST 
-- Finalmente hize algunas funciones con listas para testear las anteriores funciones
---------------------------------------------------------------------------------------

-- listaDeAPrimos dado dos naturales n y a calcula todos los a_pseudoprimos de menores a n
-- ej listaDeAPrimos  2000 4    me da los 4-pseudoprimos menores a 2000
-- [15,85,91,341,435,451,561,645,703,1105,1247,1271,1387,1581,1695,1729,1891,1905]
listaDeAPrimos:: Integer-> Integer->[Integer]
listaDeAPrimos n a =  [x | x <- [1..n] ,     es_aPseudoprimo x a]


-- todosLosCarmichael calcula todos los numeros de carmichael menores a n
-- ej todosCarmichael 10000 --> [561,1105,1729,2465,2821,6601,8911]  me da todos los carmichael menores a 10000
-- observacion
--------------
-- el calculo de estos numeros es compleja y para calular los numeros de carmichael menores a 100.000
-- como en el TP mi PC tardo una hora y media.
todosLosCarmichael:: Integer-> [Integer]
todosLosCarmichael n =  [x | x <- [1..n] ,     esCarmichael x ]


