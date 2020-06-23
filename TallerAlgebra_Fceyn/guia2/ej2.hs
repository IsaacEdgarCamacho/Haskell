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
--Escribir una funcion que sume dos vectores de R2 con y sin tuplas
sumaVectores1:: (Float,Float)->(Float,Float)->(Float,Float)
sumaVectores1 v w = (fst v + fst w , snd v + snd w)

{-Notar que no se puede hacer una funcion que me devuelva dos parametros
sino es con tuplas 
sumaVectores2 :: Float -> Float -> Float -> Float -> Float -> Float
sumaVectores2 a b c d = 
-}

--Hacer funciones que retornen la norma de un vector de R2
--con tuplas y con numeros sueltos

normaNum:: Float-> Float->Float
normaNum x y = sqrt ( x*x + y*y)

normaVector1:: (Float,Float)-> Float
normaVector1 v = sqrt( (fst v) * (fst v) + (snd v)*(snd v) )

--Ahora hacer una funcion que retorne la norma de la suma de dos 
--vectores usando las dos funciones anteriores
normaSuma1::(Float, Float)->(Float, Float)-> Float
normaSuma1 v w = normaVector1 (sumaVectores1 v w)



--Ejemplo: suma de vectores en R2

sumaVectoresR2:: (Num a, Num b)=> (a,b) -> (a, b)->(a, b)
sumaVectoresR2 v w = ( fst v + fst w , snd v + snd w)


--Codear una funcion que verifique si una tupla es el origen de R2
esOrigen:: (Float , Float )  -> Bool
esOrigen v  | fst v == 0 && snd v == 0  = True    
            | otherwise                 = False    

