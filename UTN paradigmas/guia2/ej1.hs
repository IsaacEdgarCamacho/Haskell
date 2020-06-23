{-
Definir una función ​ siguiente​ , que al invocarla con un número cualquiera me
devuelve el resultado de sumar a ese número el 1.
Main> siguiente 3
4
-}

siguiente:: Num a => a -> a
siguiente x = (x+) 1