import Data.Char (ord, chr)
-------------------------------------------------------------------------------------------------
--                                      TP2 TALLER DE ALGEBRA
-------------------------------------------------------------------------------------------------
--                              (2da parte): Sistema criptográfico RSA
-------------------------------------------------------------------------------------------------
--                              ALUMNO:     Isaac Camacho
--                              Legajo:     259/20
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
--                              FUNCIONES AUXILIARES
-------------------------------------------------------------------------------------------------

--dado dos enteros calcula el mcd
mcd :: Integer->Integer->Integer
mcd x y     | abs y > abs x     = mcd y x
            | y == 0            = abs x
            | otherwise         = mcd y (mod x y)

coprimos::Integer->Integer->Bool
coprimos x y = (mcd  x y) == 1

-- recibe un Integer y lo pasa a Char
intToChar:: Integer-> Char
intToChar x = chr (fromInteger x)

-- recibe un Char y lo pasa a Integer
charToInt:: Char-> Integer
charToInt x = fromIntegral(ord x)

-- dado x calcula los divisores de x hasta y
divisoresHasta :: Integer->Integer->Integer
divisoresHasta x y  | y == 1        = 1
                    | mod x y == 0  = 1 + divisoresHasta x (y-1)
                    | otherwise     = divisoresHasta x (y-1)


esPrimo :: Integer -> Bool
esPrimo x = divisoresHasta x x == 2

-- dado n me da la lista de todos los coprimos a n entre [2..n-1]
listacoPrimos :: Integer->[Integer]
listacoPrimos n = [x | x <- [2..n-1] , coprimos x n ]

mcdExt :: Integer -> Integer -> (Integer , Integer , Integer)
mcdExt a 0 = (1, 0, a)
mcdExt a b = (t, s - q * t, g)
       where  (q, r) = (div a b, mod a b)       
              (s, t, g) = mcdExt b r


invMod :: Integer -> Integer -> Integer
invMod a m    | x < 0 = x + m
              | otherwise = x
       where (x, _, _) = mcdExt a m

----------------------------------------------------------------------------------------
-- GENERAR CLAVE PUBLICA 
----------------------------------------------------------------------------------------

-- la forma de elejir el exponente de cifrado 
-- elije_d::Integer ->Integer
-- elije_d m = (listacoPrimos m) !! 2

elije_d::Integer ->Integer
elije_d m = (listacoPrimos m) !! 2

calculaClavePublica :: Integer->Integer->(Integer,Integer)
calculaClavePublica p q =  ( p*q,  elije_d (   (p-1)*(q-1)   ))

-- dada la clave publica (n,d) y un entero x, codifca a x
cifrar::(Integer,Integer)->Integer->Integer
cifrar (n,d) x = mod (x^d)  n

----------------------------------------------------------------------------------------
-- GENERAR CLAVE PRIVADA
----------------------------------------------------------------------------------------
-- calcula_e::Integer->Integer->Integer
-- calcula_e d phi  = invMod d phi

calcula_e::Integer->Integer->Integer
calcula_e d phi  = invMod d phi

calculaClavePrivada :: Integer->Integer->(Integer,Integer)
calculaClavePrivada p q = ( p*q  ,  calcula_e (elije_d((p-1)*(q-1))) ( (p-1)*(q-1) ) )    


-- dada la clave publica (n,d) y un entero x, decodifca a x
descifrar::(Integer,Integer)->Integer->Integer
descifrar (n,e) x = mod (x^e)  n

cifraLista :: (Integer,Integer)-> String->[Integer] -> [Integer]
cifraLista (n,d) l out 
        | (tail l)  == [] = (cifrar (n,d) (charToInt (head l)))   : out
        | otherwise       = (cifrar (n,d) (charToInt (head l)))   : cifraLista (n,d) (tail l) out


descifraLista :: (Integer,Integer)-> [Integer]->String -> String
descifraLista (n,e) l out 
        | (tail l) == []= (intToChar (descifrar (n,e) (head l) ))    : out
        | otherwise     = (intToChar (descifrar (n,e) (head l) ))    : descifraLista (n,e) (tail l) out

-------------------------------------------------------------------------------------------------
-- FUNCIONES DEL TP
-------------------------------------------------------------------------------------------------
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer)) 
generarClaves p q =  (   calculaClavePublica p q   ,calculaClavePrivada p q )

encriptar :: (Integer, Integer) -> String -> [Integer] 
encriptar (n,d) lista = cifraLista (n,d) lista []

desencriptar :: (Integer, Integer) -> [Integer] -> String 
desencriptar (n,e) lista = descifraLista (n,e) lista []

-------------------------------------------------------------------------------------------------
-- Ejercicio opcional: Romper el código asociado a la clave pública (100337, 60953), desencriptar la
-- siguiente pregunta:
-- [33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800,
-- 91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389]
-- encriptar la respuesta, y ponerla como comentario en el T.P.
-------------------------------------------------------------------------------------------------
buscaSigPrimo ::Integer->Integer
buscaSigPrimo n
        | n == 1        = 2
        | esPrimo n     = n
        | otherwise     = buscaSigPrimo (n+1)

-- dado x entero busca el siguiente primo
siguientePrimo ::Integer->Integer
siguientePrimo x = buscaSigPrimo (x+1)

primerPrimoQueDivida::Integer->Integer->Integer
primerPrimoQueDivida n y
        | esPrimo y  && (mod n y) == 0    = y
        | otherwise                       = primerPrimoQueDivida n (y+1)

-- dado un numero compuesto n y un divisor primo me devuelve (p,q)
-- p y q son los primos que usaron para encriptar
factorizar::Integer->Integer->(Integer,Integer)
factorizar n p = (p, div n p)

-- Si la clave publica es (100337, 60953) debo hallar el inverso de 60953 modulo (p-1)*(q-1)
armaClavePriv:: (Integer,Integer) -> Integer-> (Integer,Integer)
armaClavePriv (p,q) m = (p*q, invMod m ((p-1)*(q-1)))
-- ¿como obtener clave privada?
-- armaClavePriv (factorizar 100337 (primerPrimoQueDivida 100337 100)) 60953
-- ¿como desencripto?
-- desencriptar ( armaClavePriv (factorizar 100337 (primerPrimoQueDivida 100337 100)) 60953) [33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800,91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389]

-- *Main> encriptar (100337 , 60953) "napolitana con ajo y cebolla"[96986,58255,78982,22839,99961,18800,1606,58255,96986,58255,77756,74457,22839,96986,77756,58255,22329,22839,77756,61099,77756,74457,23881,11695,22839,99961,99961,58255]
-- *Main> desencriptar (100337 ,1001) [96986,58255,78982,22839,99961,18800,1606,58255,96986,58255,77756,74457,22839,96986,77756,58255,22329,22839,77756,61099,77756,74457,23881,11695,22839,99961,99961,58255]
-- "napolitana con ajo y cebolla"
-------------------------------------------------------------------------------------------------


--  TESTS
--  Finalmente agrego dos funciones que dados dos primos diferentes encripta un String.
--  ej. 
--      ej_encriptar  83 97 "Cual es tu pizza favorita?"
--      obtengo     [4822,7524,679,6594,3038,3128,3868,3038,6145,7524,3038,7207,3174,2034,2034,679,3038,3757,679,2773,3164,6149,3174,6145,679,5887]
ej_encriptar::Integer->Integer-> String-> [Integer]
ej_encriptar p q s = encriptar (calculaClavePublica  p q) s

--  ej. para desencriptar 
--      siendo el mensaje cifrado --> vector = [4822,7524,679,6594,3038,3128,3868,3038,6145,7524,3038,7207,3174,2034,2034,679,3038,3757,679,2773,3164,6149,3174,6145,679,5887] 
--  ej_desencriptar  83 97 vector 
--  obtengo     "Cual es tu pizza favorita?"
ej_desencriptar::Integer->Integer-> [Integer]-> String
ej_desencriptar p q v = desencriptar (calculaClavePrivada  p q) v


