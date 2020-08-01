import Data.Char (ord, chr)
-------------------------------------------------------
--             TP2 TALLER DE ALGEBRA
-------------------------------------------------------
-- (2da parte): Sistema criptográfico RSA
-------------------------------------------------------
-- ALUMNO:     Isaac Camacho
-- Legajo:     259/20
-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------
--  FUNCIONES AUXILIARES
-------------------------------------------------------

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

-- dado n > 0 me da la lista de primos desde 2 hasta n-1
listaPrimos :: Integer->[Integer]
listaPrimos n = [x | x <- [2..n-1] , (esPrimo x) ]

-- dado n me da la lista de todos los coprimos a n entre [2..n-1]
listacoPrimos :: Integer->[Integer]
listacoPrimos n = [x | x <- [2..n-1] , coprimos x n ]

----------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------
-- GENERAR CLAVE PUBLICA 
----------------------------------------------------------------------------------------
elije_d::Integer ->Integer
elije_d m = (listacoPrimos m) !! 2


calculaClavePublica :: Integer->Integer->(Integer,Integer)
calculaClavePublica p q =  ( p*q,  elije_d (   (p-1)*(q-1)   ))

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- dada la clave publica (n,d) y un entero x, codifca a x
cifrar::(Integer,Integer)->Integer->Integer
cifrar (n,d) x = mod (x^d)  n




----------------------------------------------------------------------------------------
-- GENERAR CLAVE PRIVADA
----------------------------------------------------------------------------------------
-- mcdExt :: Integer -> Integer -> (Integer , Integer , Integer)
-- mcdExt a b | b > a = mcdExt b a
-- mcdExt a 0 = (a, 1, 0)
-- mcdExt a b = (d, t, s - t * k)
--        where  (k, r) = (div a b, mod a b)       
--               (d, s, t) = mcdExt b r

-- third :: (Integer,Integer,Integer)->Integer
-- third (_,_,x) = abs x

mcdExt :: Integral a => a -> a -> (a,a,a)
mcdExt a 0 = (1, 0, a)
mcdExt a b = (t, s - q * t, g)
       where  (q, r) = a `quotRem` b
              (s, t, g) = mcdExt b r

invMod :: Integer -> Integer -> Integer
invMod a m    | x < 0 = x + m
              | otherwise = x
       where (x, _, _) = mcdExt a m


listaDecoPrimos :: Integer -> [Integer]
listaDecoPrimos n = [x | x <- [1..n] , coprimos  n x]

-- calcula_e::Integer->Integer->Integer->Integer
-- calcula_e d phi k = div (1+ k*phi) d


calcula_e::Integer->Integer->Integer
calcula_e d phi  = invMod d phi

calculaClavePrivada :: Integer->Integer->(Integer,Integer)
calculaClavePrivada p q = ( p*q  ,  calcula_e (elije_d((p-1)*(q-1))) ( (p-1)*(q-1) ) )    


-- dada la clave publica (n,d) y un entero x, decodifca a x
descifrar::(Integer,Integer)->Integer->Integer
descifrar (n,e) x = mod (x^e)  n


----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-- Precondiion p, q son primos
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer)) 
generarClaves p q =  (   calculaClavePublica p q   ,calculaClavePrivada p q )



cifraLista :: (Integer,Integer)-> String->[Integer] -> [Integer]
cifraLista (n,d) l out 
        | (tail l)  == [] = (cifrar (n,d) (charToInt (head l)))   : out
        | otherwise       = (cifrar (n,d) (charToInt (head l)))   : cifraLista (n,d) (tail l) out


--encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar :: (Integer, Integer) -> String -> [Integer] 
encriptar (n,d) lista = cifraLista (n,d) lista []
  

descifraLista :: (Integer,Integer)-> [Integer]->String -> String
descifraLista (n,e) l out 
        | (tail l) == []= (intToChar (descifrar (n,e) (head l) ))    : out
        | otherwise     = (intToChar (descifrar (n,e) (head l) ))    : descifraLista (n,e) (tail l) out


desencriptar :: (Integer, Integer) -> [Integer] -> String 
desencriptar (n,e) lista = descifraLista (n,e) lista []
        
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
--- OPCIONAL ROMPER CLAVE
-- dado un entero n que no sea primo busca el siguiente primo
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
-- obtener clave privada
-- armaClavePriv (factorizar 100337 (primerPrimoQueDivida 100337 100)) 60953


-- como desencripto
-- desencriptar ( armaClavePriv (factorizar 100337 (primerPrimoQueDivida 100337 100)) 60953) [33706, 38913, 58255, 99961, 77756, 23881, 220, 77756, 1606, 38913, 77756, 78982, 18800,91658, 91658, 58255, 77756, 96593, 58255, 438, 22839, 28700, 18800, 1606, 58255, 48389]

-- *Main> encriptar (100337 , 60953) "napolitana con ajo y cebolla"[96986,58255,78982,22839,99961,18800,1606,58255,96986,58255,77756,74457,22839,96986,77756,58255,22329,22839,77756,61099,77756,74457,23881,11695,22839,99961,99961,58255]
-- *Main> desencriptar (100337 ,1001) [96986,58255,78982,22839,99961,18800,1606,58255,96986,58255,77756,74457,22839,96986,77756,58255,22329,22839,77756,61099,77756,74457,23881,11695,22839,99961,99961,58255]
-- "napolitana con ajo y cebolla"
-------------------------------------------------------------------------------------------------
--  TESTS
encrip1::Integer->Integer-> String-> [Integer]
encrip1 p q s = encriptar (calculaClavePublica  p q) s

desencrip1::Integer->Integer-> [Integer]-> String
desencrip1 p q v = desencriptar (calculaClavePrivada  p q) v


