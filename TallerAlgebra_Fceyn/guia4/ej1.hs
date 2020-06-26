--implementar la rumatoria desde 1.....m
sumatoria :: Integer ->Integer
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)


--f1(n) = Sumatoria desde 0 a n de 2^n
sumaPot :: Integer -> Integer
sumaPot 0 = 1
sumaPot n = 2^n + sumaPot(n-1)

-- f2 (n q) = suma las potencias de base q y exponente 1..n
f3:: Integer->Integer->Integer
f3 1 q = q
f3 n q = q^n + (f3 ( n-1) q)