import Data.Char (ord, chr)


caracter::[Char]-> Char
caracter (x:xs) = head xs

mayor :: Integer->Integer->Bool
mayor x y = x < y

intToChar:: Integer-> Char
intToChar x = chr (fromInteger x)


charToInt:: Char-> Integer
charToInt x = fromIntegral(ord x)

---------------------------------------------------------
-- | Division de numeros naturales_0 : a ` divNat ` d = a `div ` d
divNat :: Int -> Int -> Int
divNat a d  | a < d = 0
            | otherwise = (a - d ) ` divNat ` d + 1
-- | Resto de numeros naturales_0 : a ` modNat ` d = a `mod ` d
modNat :: Int -> Int -> Int
modNat a d = a - d *( a ` divNat ` d )


modulo :: Int -> Int -> Int
modulo a d  | a >= 0 || r' == 0 = r'
            | otherwise = abs d - r'
            where r' = abs a ` modNat ` abs d


-- Esquema extendido de Euclides recursivo en Haskell: Dados a y b no
-- negativos y no ambos nulos, devuelve (d 0 , s 0 , t 0 ) tales que d 0 = (a : b) =
-- s 0 · a + t 0 · b .

mcdExt2 :: Integer -> Integer -> (Integer , Integer , Integer)
mcdExt2 a b | b > a = mcdExt b a
mcdExt2 a 0 = (a, 1, 0)
mcdExt2 a b = (d, t, s - t * k)
       where  (k, r) = (div a b, mod a b)       
              (d, s, t) = mcdExt2 b r


-- mcdExt :: Integral a => a -> a -> (a,a,a)
-- mcdExt a 0 = (1, 0, a)
-- mcdExt a b = (t, s - q * t, g)
--         where (q, r) = a `quotRem` b
--               (s, t, g) = mcdExt b r






invMod2 :: Integer -> Integer -> Maybe Integer
invMod2 n p | m /= 1    = Nothing
            | x < 0     = Just (x + p)
            | otherwise = Just x
    where (x,_,m) = mcdExt n p
 
-- [Algoritmo extendido de Euclides]
-- (mcd a b) es la terna (x,y,g) tal que g es el máximo común divisor
-- de a y b y se cumple que ax + by = g. Por ejemplo,
--    mcdExt  2  5  ==  (-2,1,1)
--    mcdExt  2  6  ==  ( 1,0,2)
--    mcdExt 12 15  ==  (-1,1,3)
-- mcdExt :: Integer -> Integer -> (Integer,Integer,Integer)
-- mcdExt a 0 = (1, 0, a)
-- mcdExt a b = (t, s - q * t, g)
--   where (q, r)    = a `quotRem` b
--         (s, t, g) = mcdExt b r
 

mcdExt :: Integral a => a -> a -> (a,a,a)
mcdExt a 0 = (1, 0, a)
mcdExt a b = (t, s - q * t, g)
       where  (q, r) = a `quotRem` b
              (s, t, g) = mcdExt b r

invMod :: Integer -> Integer -> Integer
invMod a m    | x < 0 = x + m
              | otherwise = x
       where (x, _, _) = mcdExt a m
       