{-
Signatura y Tipos de datos
-}

--Escribir una funcion con una signatura que devuelva el primer parametro
--y que acepte cualquier tipo de parametros

primero:: t-> u -> t
primero x _ = x


--Escribir una funcion mismoTipo que devuelve True si los dos parametros son de
--distinto tipo
mismoTipo:: a -> b -> Bool
mismoTipo _ _  = True

--Escribir una funcion que devuelve el triple de un numero
-- el dominio de la funcion es solamente los numericos porque 
-- la operacion multiplicacion solo se puede aplicar a numeros
triple :: Num t => t -> t
triple x = 3 * x

-- ¿Qué tipo tienen las siguientes funciones?
maximo:: (Ord t)=>t->t->t --compara a los dos argumentos de manera que debe ser Ordenable
maximo x y  | x >= y = x
            | otherwise = y

distintos:: (Eq a)=>a-> a->Bool -- como los compara, los parametros deben ser Eq
distintos x y = x /= y
