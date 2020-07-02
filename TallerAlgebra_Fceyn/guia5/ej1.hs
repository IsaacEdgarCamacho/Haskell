prod :: Integer->Integer->Integer
prod d h    | d == h    = d
            | otherwise = h * prod d (h-1)

--otra version
prod2::Integer->Integer->Integer
prod2 d h   |   d == h      = h
            |   otherwise   = d * prod2 (d+1) h

factorial :: Integer->Integer
factorial n = prod 1 n



sumaDivisoresHasta :: Integer->Integer->Integer
sumaDivisoresHasta _ 1 = 1
sumaDivisoresHasta x y = if (mod x y == 0) then y +sumaDivisoresHasta x (y-1)
                         else sumaDivisoresHasta x (y-1)


sumaDivisores :: Integer-> Integer
sumaDivisores n = sumaDivisoresHasta n n


cantDivisores ::Integer->Integer->Integer
cantDivisores x y   | y == 1        = 1
                    | mod x y == 0  = 1 + cantDivisores x (y-1)
                    | otherwise     = cantDivisores x (y-1)

esPrimo :: Integer-> Bool
esPrimo n = cantDivisores n n == 2

