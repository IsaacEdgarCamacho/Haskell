-- resta :: Int -> Int -> Int es la resta natural −, i.e., n − m = max{0, n − m}
-- menor :: Int -> Int -> Bool determina si el primer argumento es menor
-- mayor :: Int -> Int -> Bool determina si el primer argumento es mayor
-- iguales :: Int -> Int -> Bool determina si dos valores son iguales


resta :: Int -> Int -> Int
resta 0 y = y
resta x 0 = x
resta x y = resta (pred x)(pred y)


menor :: Int -> Int -> Bool
menor 0 _ = True
menor _ 0 = False
menor x y = menor (pred x) (pred y)


mayor :: Int -> Int -> Bool 
mayor _ 0 = True
mayor 0 _ = False
mayor x y = mayor (pred x) (pred y)

iguales :: Int -> Int -> Bool
iguales x y = x == y

-- Implementar una función para determinar si un número n es potencia de otro m. Está
-- permitido utilizar mod y div (mérito extra por implementarlo con una única ecuación)
-- potencia:: Integer->Integer->Bool
-- potencia n m = (div n m == 1) && potencia(div n m) m