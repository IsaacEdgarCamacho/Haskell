-- productoria :: [Int] -> Int que devuelve la productoria de los elementos.
productoria :: [Int] -> Int 
productoria []      = 1
productoria (x:xs)  = x * productoria(xs)

-- sumarN :: Int -> [Int] -> [Int] que dado un número N y una lista xs, suma N a cada
-- elemento de xs.

sumarN::Int->[Int]->[Int]
sumarN _ [] = []
sumarN n (x:xs) = x+n: sumarN n xs

sumarN2::Int->[Int]->[Int]
sumarN2 _ [] = []
sumarN2 n l = (head l) + n : sumarN2 n (tail l)


-- sumarElPrimero :: [Int] -> [Int] que dada una lista no vacı́a xs, suma el primer
-- elemento a cada elemento de xs. Ejemplo sumarElPrimero [1,2,3]--> [2,3,4]
sumarElPrimero::[Int] -> [Int]
sumarElPrimero l   = head l + head l: sumarN (head l ) (tail l)

-- sumarElUltimo :: [Int] -> [Int] que dada una lista no vacı́a xs, suma el último
-- elemento a cada elemento de xs. Ejemplo sumarElUltimo [1,2,3]-->[4,5,6]
ultimo::[Int]->Int
ultimo l
    | length l == 0     = 0
    | length l == 1     = head l
    | otherwise         = ultimo(tail l)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo []        = []
sumarElUltimo (x:xs)    =  (x + ultimo (x:xs)) :  sumarElUltimo xs