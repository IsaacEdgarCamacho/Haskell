-- Ejercicio 1
-- Dar el tipo e implementar una función sumaDistintos que dados a, b, c ∈ N >0 calcule la suma de los tres números
-- sin sumar repetidos (si los hubiera).
-- Por ejemplo:
-- sumaDistintos 2 7 3 --> 12
-- sumaDistintos 1 1 1 --> 1
-- sumaDistintos 2 3 2 --> 5
sumaDistintos::Integer->Integer->Integer->Integer
sumaDistintos a b c 
            | a == b && b == c      = a
            | a == b && b /= c      = b+c
            | a == c && b /= c      = a+b
            |otherwise              = a+b+c