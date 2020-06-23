--hacer la funcion fibonacci y factorial sin usar guardas

fact::Int->Int -- usando pattern maching
fact 0 = 1
fact n = n * fact (n-1)

fibo:: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo(n-2)

--ahora usando cortocircuitos
fact1 :: Int -> Int
fact1 n = (n == 0) && ()