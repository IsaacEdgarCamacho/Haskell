{-
Definir la función ​ mcm/2 que devuelva el mínimo común múltiplo entre dos números,
de acuerdo a esta fórmula.
m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)}
Más información​ .
Nota​ : Se puede utilizar gcd.
-}

-- Primero hago una funcion que me de los divisores
factores:: Integer -> [Integer]
factores n = [x | x <- [1..n], mod n x == 0] 

-- Ahora hacemos una fucnion que determine el maximo comun divisor
mcd :: Integer -> Integer -> Integer
mcd x y =  if  maximum (factores x)  `elem` (factores y)   then maximum (factores x)
                                                            else 1