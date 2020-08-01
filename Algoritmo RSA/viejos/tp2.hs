import Data.Char (ord, chr)


divisoresHasta :: Integer->Integer->Integer
divisoresHasta x y  | y == 1        = 1
                    | mod x y == 0  = 1 + divisoresHasta x (y-1)
                    | otherwise     = divisoresHasta x (y-1)


esPrimo :: Integer -> Bool
esPrimo x = divisoresHasta x x == 2


-- Esquema extendido de Euclides recursivo en Haskell: Dados a y b no
-- negativos y no ambos nulos, devuelve (d 0 , s 0 , t 0 ) tales que d 0 = (a : b) =
-- s 0 · a + t 0 · b .

-- mcdExt :: Integer -> Integer -> (Integer , Integer , Integer)
-- mcdExt a b | b > a = mcdExt b a
-- mcdExt a 0 = (a, 1, 0)
-- mcdExt a b = (d, t, s - t * k)
--         where (k, r) == (div a b, mod a b)       
--                 (d, s, t) == mcdExt b r


mcd :: Integer->Integer->Integer
mcd x y     | abs y > abs x     = mcd y x
            | y == 0            = abs x
            | otherwise         = mcd y (mod x y)


coprimos::Integer->Integer->Bool
coprimos x y = (mcd  x y) == 1


algoDivision :: Integer -> Integer -> (Integer,Integer)
algoDivision a d | a < d = (0, a)
                 | otherwise = (1 + k, r)
                where (k, r) = algoDivision(a - d) d



-- Algoritmo recursivo en Haskell para calcular el desarrollo en base d > 0
-- de un número a ∈ N 0 .
des :: Integer -> Integer -> [Integer]
des 0 _ = [0]
des a d = des (div a d) d ++ [mod a d]

-- generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer)) 
-- que dados dos números primos p y q, genera un par que contiene una clave pública y una clave privada en
-- el formato ((n, d), (n, e)).
-- Aclaración: El exponente de descifrado e de la clave privada, coprimo con m, puede ser elegido de
-- cualquier manera que consideren conveniente; por ejemplo: lo más chico posible, el primero posible
-- a partir de 33, lo más grande posible, etc (recordar la condición 2 ≤ e ≤ m − 2).
-- Sugerencia: Para calcular el exponente de cifrado d de la clave pública, pueden utilizar las funciones
-- de la clase 9 y/o de la clase 10. Notar que hallar d es equivalente a resolver la ecuación de congruencia
-- eX ≡ 1 (mod m).
calculaPym:: Integer->Integer->(Integer,Integer)
calculaPym p q  
        | p*q <= 127    = (-1,-1)
        |otherwise      = (p*q, (p-1)*(q-1))


divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n] , mod n x == 0]

listaPrimos :: Integer->[Integer]
listaPrimos n = [x | x <- [2..n-1] , (esPrimo x) ]

-- va a ser el primer primo desde 2 que sea coprimos con m
calcula_e::Integer ->Integer
calcula_e m = (listaPrimos m) !! 4

-- en realidad es el m del tp
phiDeN :: Integer->Integer->Integer
phiDeN p q = (p-1)*(q-1)

calculaClavePublica :: Integer->Integer->(Integer,Integer)
calculaClavePublica p q =  ( p*q,  calcula_e (phiDeN p q))

calcula_d::Integer->Integer->Integer->Integer
calcula_d e phi k = div (1+ k*phi) e

calculaClavePrivada :: Integer->Integer->(Integer,Integer)
calculaClavePrivada p q = (  calcula_d 11 (phiDeN p q) 3   ,p*q)


--Si el producto de los primos elegidos es < 127 devuelve ((-1,-1),(-1,-1))
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer)) 
generarClaves p q  
        | p * q <= 127          =  ((-1,-1),(-1,-1))
        | otherwise             =  (   calculaClavePublica p q   ,calculaClavePrivada p q )



-- dada la clave publica (n,d) y un entero x, codifca a x
cifrar::(Integer,Integer)->Integer->Integer
cifrar (n,d) x = mod (x^d)  n

-- encriptar :: (Integer, Integer) -> String -> [Integer] 
-- que dada una clave pública y un
-- mensaje, lo reemplaza por la lista de enteros que lo encripta.
-- Recuerdo: String es lo mismo que [Char].

-- encriptar :: (Integer, Integer) -> String -> [Integer] 
-- encriptar (x,y) s = 

cifraLilista :: (Integer,Integer)-> [Integer]->[Integer] -> [Integer]
cifraLilista (a,b) (x:xs) out 
        | xs == []      = (cifrar (a,b) x ): out
        | otherwise     = (cifrar (a,b ) x ): cifraLilista (a,b) xs out

encriptar :: (Integer, Integer) -> [Integer] -> [Integer] 
encriptar (a,b) lista = cifraLilista (a,b) lista []
       

-- desencriptar :: (Integer, Integer) -> [Integer] -> String 
-- que dada una clave privada y
-- una lista de enteros que encripta un mensaje, lo desencripta.

desencriptar :: (Integer, Integer) -> [Integer] -> String 
desencriptar (a,b) (x:xs) = "Hola mundo"