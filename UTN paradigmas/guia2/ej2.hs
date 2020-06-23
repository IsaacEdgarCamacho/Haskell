{-
Definir la función ​ mitad que al invocarla con un número cualquiera me devuelve la
mitad de dicho número, ej:
Main> mitad 5
2.5
-}

mitad :: Fractional t  => t -> t
mitad a = (a /) 2
