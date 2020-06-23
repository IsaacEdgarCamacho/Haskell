{-
Resolver la función del ejercicio 2 de la guía anterior ​ esMultiploDe/2​ , utilizando
aplicación parcial y composición.
-}

esMultiploDe :: Integer -> Integer-> Bool --VErsion original
esMultiploDe x y    | mod x y  == 0 = True 
                    | otherwise     = False    

esMultiploDe2::Integer->Integer->Bool --con aplicacion parcial
esMultiploDe2 x y = (esMultiploDe x) y

esMultiploDe3::Integer->Integer->Bool --con composicion
esMultiploDe3 x y = mod x y == 0