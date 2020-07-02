-- Dados dos enteros a, b implementar tres funciones: r 1, r 2 y r 3 que determinen si a ∼ b (a esta relacionado con b)
-- para cada uno de los siguientes casos:
-- a ∼ b sii tienen la misma paridad
-- a ∼ b sii 2a + 3b es divisible por 5
-- a ∼ b sii los dı́gitos de las unidades de a, b y ab son todos distintos

r1:: Integer->Integer->Bool
r1 x y = (mod x 2 == 0) && (mod y 2 == 0) || (mod x 2 == 1) && (mod y 2 == 1)

r2 ::Integer->Integer->Bool
r2 x y = (2*x + 3*y) `mod` 5 == 0



r3::Integer->Integer->Bool
r3 x y = (mod x 10 ) /=  (mod y 10)
