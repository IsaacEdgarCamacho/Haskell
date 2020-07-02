-- Pensar las siguientes funciones
-- sumatoria :: [Int] -> Int
-- que indica la suma de los elementos de una lista.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria l = head l + sumatoria (tail l)

--longitud :: [Int] -> Int
-- que indica cuántos elementos tiene una lista.
longitud::[Int] -> Int
longitud [] = 0
longitud l  = 1 + longitud(tail l)


-- pertenece :: Int -> [Int] -> Bool
-- que indica si un elemento aparece en la lista. Por ejemplo:
-- pertenece 9 []
-- False
-- pertenece 9 [1,2,3]
-- False
-- pertenece 9 [1,2,9,9,-1,0]
-- True
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x c  = (x == head c)  ||  pertenece x (tail c)