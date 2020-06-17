-- Escribir una funcion cantidadDeSoluciones
-- con su signatura.


--Las signatura debe cumplir con las restricciones de las operaciones sobre los argumentos

cantidadDeSoluciones:: ( Num t,Ord t) => t-> t-> Int 
cantidadDeSoluciones b c    | discri == 0   = 1
                            | discri >  0   = 2
                            | otherwise     = 0
    where discri = b^2 - 4*c

-------------------------
--  TUPLASS
-------------------------


--Ejemplo: suma de vectores en R2

sumaVectoresR2:: (Num a, Num b)=> (a,b) -> (a, b)->(a, b)
sumaVectoresR2 v w = ( fst v + fst w , snd v + snd w)


--Codear una funcion que verifique si una tupla es el origen de R2
esOrigen:: (Float , Float )  -> Bool
esOrigen v  | fst v == 0 && snd v == 0  = True    
            | otherwise                 = False    

