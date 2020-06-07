{-
Definir la función ​ esMultiploDe/2​ , que devuelve True si el segundo es múltiplo
del primero, p.ej.
Main> esMultiplo 12 3
True
-}
--Primero resuelvo con gardas
esMultiploDe_v1:: Integer -> Integer -> Bool
esMultiploDe_v1 x y | mod x y == 0 = True
                 | otherwise    = False

-- Resolvemos con condifionales
esMultiploDe_v2:: Integer -> Integer -> Bool
esMultiploDe_v2 x y = if mod x y == 0  then True    
                                       else False
