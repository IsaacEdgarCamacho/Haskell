-- Pensar las siguientes funciones
-- sumatoria :: [Int] -> Int
-- que indica la suma de los elementos de una lista.

sumatoria::[Integer]->Integer
sumatoria [] = 0
sumatoria (x:ls) = x + sumatoria ls

-- sumatoria :: [Int] -> Int
-- sumatoria [] = 0
-- sumatoria l = head l + sumatoria (tail l)

--longitud :: [Int] -> Int
-- que indica cuántos elementos tiene una lista.

longitud:: [Integer]->Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- longitud::[Int] -> Int
-- longitud [] = 0
-- longitud l  = 1 + longitud(tail l)


-- pertenece :: Int -> [Int] -> Bool
-- que indica si un elemento aparece en la lista. Por ejemplo:
-- pertenece 9 []
-- False
-- pertenece 9 [1,2,3]
-- False
-- pertenece 9 [1,2,9,9,-1,0]
-- True
pertenece::Integer->[Integer]->Bool
pertenece _ [] = False
pertenece e (x:xs) = x == e || pertenece e xs 


-- pertenece :: Int -> [Int] -> Bool
-- pertenece _ [] = False
-- pertenece x c  = (x == head c)  ||  pertenece x (tail c)

-- ¿podemos definir una función máximo que funcione por igual para 0,
-- 10 o una cantidad n de elementos?
maximo::[Integer] -> Integer
maximo [] = 0
maximo (x:xs)
    | length xs == 0 = x
    | x < head xs    = maximo (xs)
    | otherwise      = maximo (x : tail xs)
