{-
Definir la función ​ esBisiesto/1​ , indica si un año es bisiesto. (Un año es bisiesto
si es divisible por 400 o es divisible por 4 pero no es divisible por 100) Nota:
Resolverlo reutilizando la función ​ esMultiploDe/2
-}

-- Resolvemos con donciciones logicas
esBisiesto_v1 :: Integer -> Bool
esBisiesto_v1 x = (mod x 400 == 0 || mod x 4 == 0) && not (mod x 100 == 0)

-- Algo tribial pero es bueno recordad que podes renombrar a las funciones
esBisiesto_v2 :: Integer -> Bool
esBisiesto_v2 = esBisiesto_v1

