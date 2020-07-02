-- Ejercicio
-- I Escribir una expresión que denote la lista estrictamente decreciente de enteros que comienza
-- con el número 1 y termina con el número -100.

listaDecreciente::Int->[Int]
listaDecreciente (-100) = (-100) : []
listaDecreciente n = n : listaDecreciente (n-1) 



-- I Definir la función primerMultiplode45345 :: [Int] -> Int que indica el primer
-- elemento de la lista que es múltiplo de 45345 que encuentre en la lista.

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l 
                | length l == 0  = 1
                | head l `mod` 45345 == 0   = head l
                | otherwise                 = primerMultiplode45345 (tail l)