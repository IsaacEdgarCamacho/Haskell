-- Implementar la función fib : Z ≥0 → Z que devuelve el i-ésimo número de Fibonacci.

fibonacci::Integer->Integer
fibonacci n
        | n == 0    = 0
        | n == 1    = 1
        | otherwise = fibonacci (n-1) + fibonacci (n-2)

--definir el factorial
fact::Integer->Integer
fact n
    |n==0       = 1
    |otherwise  = n * fact (n-1)

-- mplementar funciones recursivas para calcular el n-ésimo término de las siguientes
-- sucesiones a 1 = 2, a n+1 = 2 n a n + 2 n+1 n!, para todo n ∈ N.

s1::Integer-> Integer
s1 n
    | n == 1    = 2
    | otherwise = 2*n * s1(n-1) + 2^(n+1) * fact(n)


-- a 1 = 1, a 2 = 2 y a n+2 = na n+1 + 2(n + 1)a n , para todo n ∈ N.
s2::Integer->Integer
s2 n
    | n == 1    = 1
    | n == 2    = 2
    | otherwise = n * s2 (n-1) + 2 * (n+1)* s2(n-2)