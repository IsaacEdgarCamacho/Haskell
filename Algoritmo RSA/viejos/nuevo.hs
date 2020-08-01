import Data.Char (ord, chr)

----------------------------------------------------------------------------------------
--  FUNCIONES AUXILIARES
----------------------------------------------------------------------------------------
divisoresHasta :: Integer->Integer->Integer
divisoresHasta x y  | y == 1        = 1
                    | mod x y == 0  = 1 + divisoresHasta x (y-1)
                    | otherwise     = divisoresHasta x (y-1)


esPrimo :: Integer -> Bool
esPrimo x = divisoresHasta x x == 2



listaPrimos :: Integer->[Integer]
listaPrimos n = [x | x <- [2..n-1] , (esPrimo x) ]
----------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------
-- GENERAR CLAVE PUBLICA 
----------------------------------------------------------------------------------------
calcula_d::Integer ->Integer
calcula_d m = (listaPrimos m) !! 4


calculaClavePublica :: Integer->Integer->(Integer,Integer)
calculaClavePublica p q =  ( p*q,  calcula_d (   (p-1)*(q-1)   ))

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- dada la clave publica (n,d) y un entero x, codifca a x
cifrar::(Integer,Integer)->Integer->Integer
cifrar (n,d) x = mod (x^d)  n




----------------------------------------------------------------------------------------
-- GENERAR CLAVE PRIVADA
----------------------------------------------------------------------------------------
calcula_e::Integer->Integer->Integer->Integer
calcula_e d phi k = div (1+ k*phi) d


calculaClavePrivada :: Integer->Integer->(Integer,Integer)
calculaClavePrivada p q = ( p*q  ,  calcula_e 11 ( (p-1)*(q-1) ) 3  )    


-- dada la clave publica (n,d) y un entero x, decodifca a x
descifrar::(Integer,Integer)->Integer->Integer
descifrar (n,e) x = mod (x^e)  n

----------------------------------------------------------------------------------------
--  
----------------------------------------------------------------------------------------

--Si el producto de los primos elegidos es < 127 devuelve ((-1,-1),(-1,-1))
generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer)) 
generarClaves p q  
        | p * q <= 127          =  ((-1,-1),(-1,-1))
        | otherwise             =  (   calculaClavePublica p q   ,calculaClavePrivada p q )


cifraLista :: (Integer,Integer)-> String->[Integer] -> [Integer]
cifraLista (n,d) (x:xs) out 
        | xs == []      = (cifrar (n,d) (fromIntegral(ord x)) ): out
        | otherwise     = (cifrar (n,d ) (fromIntegral(ord x) )): cifraLista (n,d) xs out


--encriptar :: (Integer, Integer) -> String -> [Integer]
encriptar :: (Integer, Integer) -> String -> [Integer] 
encriptar (n,d) lista = cifraLista (n,d) lista []
  

descifraLista :: (Integer,Integer)-> [Integer]->String -> String
descifraLista (n,e) (x:xs) out 
        | xs == []      = ( chr    (fromIntegral   (descifrar (n,e) x ))   ) : out
        | otherwise     = (chr (fromIntegral (descifrar (n,e ) x )  )): descifraLista (n,e) xs out

desencriptar :: (Integer, Integer) -> [Integer] -> String 
desencriptar (n,e) lista = descifraLista (n,e) lista []
        
