-- Implementar una función para determinar si un número es múltiplo de 3. No está permitido
-- utilizar mod ni div

esMultiplode3::Integer->Bool
esMultiplode3 1 = True
esMultiplode3 0 = False
esMultiplode3 n = esMultiplode3 (n-2)


-- Implementar la función diabolico :: Int -> Bool que determine si todos los dı́gitos de
-- un número son 6

diabolico :: Int -> Bool -- usando Pattern mathing
diabolico 6 = True 
diabolico n = (mod n 10 == 6) && diabolico(div n 10) 


-- Extender la función anterior para determinar si todos los dı́gitos de un número son iguales
digitosIguales :: Int -> Bool
digitosIguales n = ( n == n`mod`10) ||(digitosIguales(n`div`10) &&(div(n`mod`100)10) == (n`mod`10))

