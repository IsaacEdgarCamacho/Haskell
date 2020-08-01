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



--------------------------------------------------------------------
listarCoprimos::Integer->Integer->[Integer]
listarCoprimos x y 
        |  y == x-1                =  [y]
        |  coprimos x y            =  y : listarCoprimos x (y+1)
        |  otherwise               =  listarCoprimos x (y+1)

coprimoList:: Integer->[Integer]
coprimoList n = listarCoprimos n 2
---------------------------------------------------------------------



-- funcion que resuelve la ecuacion diofantica ax + by = 1
-- euclideExt :: Integer -> Integer -> (Integer , Integer , Integer)
-- euclideExt a 0 = (1, 0, a)
-- euclideExt a b = (t, s - q * t, g)
--        where  (q, r) = (div a b, mod a b)       
--               (s, t, g) = euclideExt b r

euclideExt :: Integer -> Integer -> (Integer , Integer , Integer)
euclideExt a 0 = (1, 0, a)
euclideExt a b = (t, s - (div a b) * t, g)
       where  (s, t, g) = euclideExt b (mod a b)



-- dado un numero a halla el inverso de a modulo m
inversoModulo :: Integer -> Integer -> Integer
inversoModulo a m    
        | x < 0 = x + m
        | otherwise = x
       where (x, _, _) = euclideExt a m

----------------------------------------------------------------------------------------
-- GENERAR CLAVE PUBLICA 
----------------------------------------------------------------------------------------
-- Para calcular el exponente de cifrado d hallo el inverso de e modulo phi = (p-1)(q-1)
calcula_d::Integer->Integer->Integer
calcula_d e phi  = inversoModulo e phi


calculaClavePublica :: Integer->Integer->(Integer,Integer)
calculaClavePublica p q =  ( p*q, calcula_d (elije_e((p-1)*(q-1))) ( (p-1)*(q-1) ) )    

-- dada la clave publica (n,d) y un entero x, codifca a x
cifrar::(Integer,Integer)->Integer->Integer
cifrar (n,d) x = mod (x^d)  n

----------------------------------------------------------------------------------------
-- GENERAR CLAVE PRIVADA
----------------------------------------------------------------------------------------
-- El exponente de descifrado e de la clave privada, coprimo con m, puede ser elegido de
-- cualquier manera que consideren conveniente; por ejemplo: lo más chico posible en particular
-- lo elegi el segundo coprimo a partir de 2
elije_e::Integer ->Integer
elije_e m = (listacoPrimos m) !! 2

calculaClavePrivada :: Integer->Integer->(Integer,Integer)
calculaClavePrivada p q = ( p*q  ,  elije_e ((p-1)*(q-1))  )    

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
-- Para resolver el ejercicio, lo que hize fue buscar p, el primer divisor primo de 100337 y me fije si
-- el resultado de (100337 / p) era primo, asi obtuve p = 269 y q = 373, luego con (p,q) obtengo m = (p-1)*(q-1)
-- como tenemos el exponente de cifrado d = 60953 ,  hallo su inverso modulo m, con lo que obtengo e = 1001
-- entonces obtuve su clave privada (n,e) = (100337,1001), usando las funciones del tp descifre el mensaje y obtuve
-- "Cual es tu pizza favorita?"
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
armaClavePriv (p,q) m = (p*q, inversoModulo m ((p-1)*(q-1)))
-- ¿como obtener clave privada?
-- armaClavePriv (factorizar 100337 (primerPrimoQueDivida 100337 100)) 60953
-- ¿como desencripto?
-- desencriptar ( armaClavePriv (factorizar 100337 (primerPrimoQueDivida 100337 100)) 60953) [33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800,91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389]
-- obtuve: "Cual es tu pizza favorita?"

---Respuesta
-- encriptar (100337 , 60953) "napolitana con ajo y cebolla"
-- Obtengo  [96986,58255,78982,22839,99961,18800,1606,58255,96986,58255,77756,74457,22839,96986,77756,58255,22329,22839,77756,61099,77756,74457,23881,11695,22839,99961,99961,58255]
-- usar esta clave para desencriptar (100337 ,1001)

-- RESPUESTA CIFRADA
--[96986,58255,78982,22839,99961,18800,1606,58255,96986,58255,77756,74457,22839,96986,77756,58255,22329,22839,77756,61099,77756,74457,23881,11695,22839,99961,99961,58255]
-------------------------------------------------------------------------------------------------


--  TESTS
--  Finalmente agrego dos funciones que dados dos primos diferentes encripta un String.
--  ej. 
--      ej_encriptar  83 97 "Cual es tu pizza favorita?"
--      obtengo     [6368,1695,7275,584,7826,7881,6996,7826,5695,1695,7826,4697,3422,1070,1070,7275,7826,6218,7275,5134,3066,4425,3422,5695,7275,6286]
ej_encriptar::Integer->Integer-> String-> [Integer]
ej_encriptar p q s = encriptar (calculaClavePublica  p q) s

--  ej. para desencriptar 
--      siendo el mensaje cifrado --> vector = [6368,1695,7275,584,7826,7881,6996,7826,5695,1695,7826,4697,3422,1070,1070,7275,7826,6218,7275,5134,3066,4425,3422,5695,7275,6286]
--  ej_desencriptar  83 97 vector 
--  obtengo     "Cual es tu pizza favorita?"
ej_desencriptar::Integer->Integer-> [Integer]-> String
ej_desencriptar p q v = desencriptar (calculaClavePrivada  p q) v


