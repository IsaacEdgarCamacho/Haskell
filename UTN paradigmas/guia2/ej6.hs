{-
Resolver la función del ejercicio 2 de la guía anterior ​ esMultiploDe/2​ , utilizando
aplicación parcial y composición.
-}
type Persona = (String, Integer, String)

edad :: Persona-> Integer
edad ( _ , y , _ )  = y

mayorEdad::Integer -> Bool
mayorEdad x = x >= 18

esMayorDeEdad :: Persona -> Bool
esMayorDeEdad = mayorEdad . edad