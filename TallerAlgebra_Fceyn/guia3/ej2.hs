--Escribir la funcion sumatoria que sime los primeros n naturales
-- usar siempre recursion

sumatoria::Integer -> Integer
sumatoria 2 = 3
sumatoria n = n + sumatoria (n-1)

productoria::Integer->Integer
productoria 1 = 1
productoria n = n * productoria (n-1)

-- En todos los ejercicios los números son naturales (incluyendo 0)
-- 1 Implementar la función tribonacci :: Int -> Int que computa
-- 
-- T (n) = n n ≤ 2
-- T (n − 1) + T (n − 2) + T (n − 3) n > 2

tribonacci :: Integer->Integer
tribonacci n    | n<=2      =   n
                | otherwise = tribonacci(n-1)+tribonacci(n-2)+tribonacci(n-3)


-- Implementar una función para determinar si un número es múltiplo de 3. No está permitido
-- utilizar mod ni div

esMultiploDe3::Integer-> Bool -- usando pattern mathing
esMultiploDe3 0 = True
esMultiploDe3 1 = False
esMultiploDe3 2 = False
esMultiploDe3 x = esMultiploDe3(x-3)

esMultDe3::Integer->Bool
esMultDe3 x |   x == 0      =   True 
            |   x == 1      =   False
            |   x == 2      =   False
            |   x < 0       =   esMultDe3(x+3)
            |   otherwise   =   esMultDe3(x-3)