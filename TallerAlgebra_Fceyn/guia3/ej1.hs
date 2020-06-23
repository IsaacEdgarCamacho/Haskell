--hacer una funcion que determine el fibonacci de un numero

fibo :: Int->Int
fibo x  | x <= 1    = 1
        | otherwise = fibo (x-1)  + fibo (x-2)


-- hacer una funcion factorial
fact :: Int -> Int
fact n  | n < 1     = 1
        | otherwise = n * fact (n-1)

--Escribir una funcion sumar que solo use pred y succ
sumar::Integer->Integer->Integer
sumar 0 y = y
sumar x 0 = x
sumar x y = sumar (succ x) (pred y)

restar :: Integer -> Integer-> Integer
restar 0 y = y
restar x 0 = x
restar x y = restar (pred x) (pred y)


multip::Integer->Integer->Integer
multip 0 _ = 0
multip _ 0 = 0
multip x y = x + multip x (y-1 )

-- Hacer una funcion division entera sin usar div o mod y resto
division :: Integer -> Integer ->Integer
division x y    | x < y         = 0
division x y    | x == y        = 1
division x y    | otherwise     = 1 + division (x-y) y

resto :: Integer -> Integer ->Integer
resto x y    | x < y         = x
resto x y    | x == y        = 0
resto x y    | otherwise     = resto (x-y) y

