{-
Definir una función ​ triple​ , que invocando a la función con un número cualquiera
me devuelva el triple del mismo.
Main> triple 5
15
-}

triple :: Num a => a->a
triple x = 3 * x