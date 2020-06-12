{-
Definir una función ​ esNumeroPositivo​ , que invocando a la función con un número
cualquiera me devuelva true si el número es positivo y false en caso contrario.
Main> esNumeroPositivo (-5)
False
Main> esNumeroPositivo 0.99
True
-}

esNumeroPositivo:: Float-> Bool
esNumeroPositivo x  | x >= 0        = True
                    | otherwise     = False