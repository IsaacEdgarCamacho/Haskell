{-
Primera ejercitacion en haskell
-}

doble::Num a => a -> a
doble x = 2 * x


-- Algoritmo sucesor para sumar dos números: Voy sumando 1 al primero y restando uno al
-- segundo, hasta que el segundo llegue a 0. Pero, ¿qué pasa si sumamos 15 y -5?

suma_sucesor::Integer->Integer->Integer
suma_sucesor x 0 = x
suma_sucesor x y = suma_sucesor (x+1) (y-1) -- el problema es que si el argumento es < 0 no termina


suma_sucesor2::Integer->Integer->Integer
suma_sucesor2 x y   | x == 0    = y
                    | y == 0    = x
                    | y < 0     = suma_sucesor2 (x-1) (y+1)
                    | x < 0     = suma_sucesor2 (x+1) (y-1)
                    | otherwise = suma_sucesor2 (x+1)(y-1)


--Ahora podemos definir la norma vectorial un poco más claramente:                    
normaVectorial::(Float,Float)->Float
normaVectorial a = sqrt( (fst a )^2 + (snd a)^2)-- tira un warning porque el exponente se aplica a enteros.


-- crearPar :: a -> b -> (a, b) que crea un par a partir de sus dos componentes.
-- invertir :: (a, b) -> (b, a) que invierte el par pasado como parámetro
-- distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float.
crearPar :: a -> b -> (a, b)
crearPar x y = (x,y)

invertir :: (a, b) -> (b, a)
invertir (x,y) = (y,x)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos x y = sqrt( (fst x - fst y)^2 + (snd x + snd y)^2 )


-- Implementar las siguientes funciones del Ejercicio 32 Práctica 1 (reemplazamos N por Z),
-- usando tipo Integer para los números enteros y tipo Float para los números reales:
-- I 32.iii) f 1 : R → R 3 , f 1(x) = (2x, x 2 , x − 7)
f1::Float->(Float,Float,Float)
f1 x = (2*x,x^2,x-7)

--f2 : Z → Z, f 2(n) = n/2 si n es par y (n + 1) si n es impar
f2:: Integer->Integer
f2 n    | mod n 2 == 0  = div n 2
        | otherwise     = n+1


-- Implementar las funciones f y g del Ejercicio 33.i) Práctica
f:: Integer->Integer
f n     | mod n 6 == 0  = div (n^2) 2
        | otherwise     = 3*n+1

g::Integer->Integer->Integer
g n m = n*(m+1)

inv :: Float -> Float
inv x | x /= 0 = 1/x